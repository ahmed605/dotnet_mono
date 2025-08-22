use core::ffi::*;
use crate::nob::*;
use crate::crust::libc::*;
use crate::crust::assoc_lookup_cstr;
use crate::lexer::*;
use crate::missingf;
use crate::ir::*;
use crate::arena;
use crate::targets::TargetAPI;
use crate::params::*;
use crate::shlex::*;
use core::mem::zeroed;

extern "C" {
    fn rand() -> c_int;
}

pub unsafe fn load_arg(loc: Loc, arg: Arg, output: *mut String_Builder, _data: *const [u8]) {
    match arg {
        Arg::Bogus           => unreachable!("bogus-amogus"),
        Arg::AutoVar(index) => {
            sb_appendf(output, c!("        ldloc V_%zu\n"), index);
        }
        Arg::Deref(index)       => {
            sb_appendf(output, c!("        ldloc V_%zu\n"), index);
            sb_appendf(output, c!("        ldind.i8\n"));
        }
        Arg::RefAutoVar(..)  => missingf!(loc, c!("RefAutoVar\n")),
        Arg::RefExternal(name) => {
            sb_appendf(output, c!("        ldsflda int64 Program::'%s'\n"), name);
        }
        Arg::External(name)    => {
            sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), name);
        }
        Arg::Literal(literal) => {
            sb_appendf(output, c!("        ldc.i8 %zd\n"), literal);
        }
        Arg::DataOffset(offset) => {
            sb_appendf(output, c!("        ldsflda valuetype '<BLangDataSection>'/'DataSection' '<BLangDataSection>'::'Data'\n"));
            sb_appendf(output, c!("        ldc.i8 %zd\n"), offset);
            sb_appendf(output, c!("        add\n"));
        },
    }
}

pub unsafe fn call_arg(loc: Loc, fun: Arg, out: *mut String_Builder, arity: usize, funcs: *const [Func]) {
    match fun {
        Arg::Bogus           => unreachable!("bogus-amogus"),
        Arg::AutoVar(..)     => missingf!(loc, c!("AutoVar\n")),
        Arg::Deref(..)       => missingf!(loc, c!("Deref\n")),
        Arg::RefAutoVar(..)  => missingf!(loc, c!("RefAutoVar\n")),
        Arg::RefExternal(..) => missingf!(loc, c!("RefExternal\n")),
        Arg::External(name)  => {
            let mut is_local_func = false;
            for i in 0..funcs.len() {
                let func = (*funcs)[i];
                if strcmp(func.name, name) == 0 {
                    is_local_func = true;
                    break;
                }
            }

            if is_local_func {
                sb_appendf(out, c!("        call int64 class Program::'%s'("), name); // If the function we want to call collides with a instruction
                // we will get a syntax error so '' are necessary.
            }
            else {
                sb_appendf(out, c!("        ldsfld native int Program::'<%s_fnptr>'\n"), name);
                sb_appendf(out, c!("        calli unmanaged cdecl int64("));
            }

            for i in 0..arity {
                if i > 0 { sb_appendf(out, c!(", ")); }
                sb_appendf(out, c!("int64"));
            }
            sb_appendf(out, c!(")\n"));
        },
        Arg::Literal(..)     => missingf!(loc, c!("Literal\n")),
        Arg::DataOffset(..)  => missingf!(loc, c!("DataOffset\n")),
    }
}

pub unsafe fn generate_function(func: Func, output: *mut String_Builder, data: *const [u8], funcs: *const [Func], variadics: *const [(*const c_char, Variadic)]) {
    sb_appendf(output, c!("    .method static int64 '%s' ("), func.name); // If the function we want to define collides with a instruction
    // we will get a syntax error so '' are necessary.
    for i in 0..func.params_count {
        if i > 0 { sb_appendf(output, c!(", ")); }
        sb_appendf(output, c!("int64"));
    }
    sb_appendf(output, c!(") {\n"), func.name);

    let mut maxstack = 0;

    if !(func.body.count == 1 && matches!((*func.body.items.add(0)).opcode, Op::Asm {..})) {
        if func.auto_vars_count > 0 {
            sb_appendf(output, c!("        .locals init ("));
            for i in 0..func.auto_vars_count {
                if i > 0 { sb_appendf(output, c!(", ")); }
                sb_appendf(output, c!("int64 V_%zu"), i + 1);
            }
            sb_appendf(output, c!(")\n"));
        }

        maxstack = if func.params_count > 8 { func.params_count } else { 8 };

        if func.params_count > 0 {
            for i in 0..func.params_count {
                sb_appendf(output, c!("        ldarg %zu\n"), i);
                sb_appendf(output, c!("        stloc V_%zu\n"), i + 1);
            }
        }
    }

    for i in 0..func.body.count {
        let op = *func.body.items.add(i);
        match op.opcode {
            Op::Bogus               => unreachable!("bogus-amogus"),
            Op::UnaryNot {result, arg}       => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        ldc.i8 0\n"));
                sb_appendf(output, c!("        ceq\n"));
                sb_appendf(output, c!("        conv.i8\n"));
                sb_appendf(output, c!("        stloc V_%zu\n"), result);
            }
            Op::Negate {result, arg}         => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        neg\n"));
                sb_appendf(output, c!("        stloc V_%zu\n"), result);
            }
            Op::Asm {stmts} => {
                for i in 0..stmts.count {
                    let stmt = *stmts.items.add(i);
                    sb_appendf(output, c!("        %s\n"), stmt.line);
                }
            }
            Op::Binop {binop, index, lhs, rhs} => {
                load_arg(op.loc, lhs, output, data);
                load_arg(op.loc, rhs, output, data);
                match binop {
                    Binop::Plus         => sb_appendf(output, c!("        add\n")),
                    Binop::Minus        => sb_appendf(output, c!("        sub\n")),
                    Binop::Mult         => {
                        sb_appendf(output, c!("        mul\n"))
                    }
                    Binop::Div          => {
                        sb_appendf(output, c!("        div\n"))
                    }
                    Binop::Mod          => {
                        sb_appendf(output, c!("        rem\n"))
                    }
                    Binop::Equal        => {
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::NotEqual     => {
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        ldc.i4.0\n"));
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::Less         => {
                        sb_appendf(output, c!("        clt\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    },
                    Binop::LessEqual    => {
                        sb_appendf(output, c!("        cgt\n"));
                        sb_appendf(output, c!("        ldc.i4.0\n"));
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::Greater      => {
                        sb_appendf(output, c!("        cgt\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::GreaterEqual => {
                        sb_appendf(output, c!("        clt\n"));
                        sb_appendf(output, c!("        ldc.i4 0\n"));
                        sb_appendf(output, c!("        ceq\n"));
                        sb_appendf(output, c!("        conv.i8\n"))
                    }
                    Binop::BitOr        => {
                        sb_appendf(output, c!("        or\n"))
                    }
                    Binop::BitAnd       => sb_appendf(output, c!("        and\n")),
                    Binop::BitShl       => {
                        sb_appendf(output, c!("        conv.i4\n")); //Shift amount must be int32 according to the CLI specification
                        sb_appendf(output, c!("        shl\n"))
                    }
                    Binop::BitShr       => {
                        sb_appendf(output, c!("        conv.i4\n")); //Shift amount must be int32 according to the CLI specification
                        sb_appendf(output, c!("        shr\n"))
                    }
                };
                sb_appendf(output, c!("        stloc V_%zu\n"), index);
            }
            Op::Index {result, arg, offset} => {
                load_arg(op.loc, arg, output, data);
                load_arg(op.loc, offset, output, data);
                sb_appendf(output, c!("        ldc.i4.8\n"));
                sb_appendf(output, c!("        mul\n"));
                sb_appendf(output, c!("        add\n"));
                sb_appendf(output, c!("        stloc V_%zu\n"), result);
            }
            Op::AutoAssign {index, arg}     => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        stloc V_%zu\n"), index);
            }
            Op::ExternalAssign {name, arg} => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), name);
            }
            Op::Store {index, arg} => {
                sb_appendf(output, c!("        ldloc V_%zu\n"), index);
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        stind.i8\n"));
            }
            Op::Funcall {result, fun, args} => {
                let mut fixed_args = 0;
                match fun {
                    Arg::External(name) | Arg::RefExternal(name) => {
                        if let Some(variadic) = assoc_lookup_cstr(variadics, name) {
                            fixed_args = (*variadic).fixed_args;
                        }
                    }
                    _ => {}
                }

                let mut id = 0;
                let mut filler_args = 0;
                let variadic_args = args.count - fixed_args;
                if fixed_args != 0 && variadic_args > 0 && fixed_args < 8 {
                    filler_args = 8 - fixed_args;

                    id = rand();
                    sb_appendf(output, c!("        ldsfld bool Program::'<IsMacOS_ARM64>'\n"));
                    sb_appendf(output, c!("        brtrue L_%d_%d\n"), op.loc.line_number, id);
                    for i in 0..args.count {
                        load_arg(op.loc, *args.items.add(i), output, data);
                    }
                    call_arg(op.loc, fun, output, args.count, funcs);
                    sb_appendf(output, c!("        stloc V_%zu\n"), result);
                    sb_appendf(output, c!("        br L_%d_%d_end\n"), op.loc.line_number, id);
                    sb_appendf(output, c!("    L_%d_%d:\n"), op.loc.line_number, id);
                    for i in 0..fixed_args {
                        load_arg(op.loc, *args.items.add(i), output, data);
                    }
                    for _i in 0..filler_args {
                        sb_appendf(output, c!("        ldc.i8 0\n"));
                    }
                }
                else {
                    fixed_args = 0;
                }

                for i in fixed_args..args.count {
                    load_arg(op.loc, *args.items.add(i), output, data);
                }

                let args_count = args.count + filler_args;
                call_arg(op.loc, fun, output, args_count, funcs);
                sb_appendf(output, c!("        stloc V_%zu\n"), result);

                if id != 0 {
                    sb_appendf(output, c!("    L_%d_%d_end:\n"), op.loc.line_number, id);
                }

                // the "+ 1" is the stack item for the function pointer used by calli
                // TODO: implement a better way for calculating additional stack space rather than hardcoding 1
                let required_stack_items = args_count + 1;
                if required_stack_items > maxstack {
                    maxstack = required_stack_items;
                }
            }
            Op::Label {label} => {
                sb_appendf(output, c!("    L%zu:\n"), label);
            }
            Op::JmpLabel {label} => {
                sb_appendf(output, c!("        br L%zu\n"), label);
            }
            Op::JmpIfNotLabel {label, arg}  => {
                load_arg(op.loc, arg, output, data);
                sb_appendf(output, c!("        brfalse L%zu\n"), label);
            }
            Op::Return {arg} => {
                if let Some(arg) = arg {
                    load_arg(op.loc, arg, output, data);
                }
                else {
                    sb_appendf(output, c!("        ldc.i8 0\n"));
                }
                sb_appendf(output, c!("        ret\n"));
            }
        }
    }

    if func.body.count < 1 || !matches!((*func.body.items.add(func.body.count - 1)).opcode, Op::Asm {..} | Op::Return {..}) {
        sb_appendf(output, c!("        ldc.i8 0\n"));
        sb_appendf(output, c!("        ret\n"));
    }

    if maxstack != 0 {
        sb_appendf(output, c!("        .maxstack %d\n"), maxstack);
    }

    sb_appendf(output, c!("    }\n"));
}

pub unsafe fn generate_funcs(funcs: *const [Func], output: *mut String_Builder, data: *const [u8], variadics: *const [(*const c_char, Variadic)]) {
    for i in 0..funcs.len() {
        let func = (*funcs)[i];
        generate_function(func, output, data, funcs, variadics);
    }
}

pub unsafe fn generate_data_section(output: *mut String_Builder, data: *const [u8]) {
    if data.len() > 0 {
        sb_appendf(output, c!(".class '<BLangDataSection>' extends [mscorlib]System.Object {\n"));
        sb_appendf(output, c!("    .class nested assembly sealed 'DataSection' extends [mscorlib]System.ValueType {\n"));
        sb_appendf(output, c!("        .pack 1\n"));
        sb_appendf(output, c!("        .size %zu\n"), data.len());
        sb_appendf(output, c!("    }\n"));
        sb_appendf(output, c!("    .field assembly static initonly valuetype '<BLangDataSection>'/'DataSection' 'Data' at DataArray\n"));
        sb_appendf(output, c!("    .data cil DataArray = bytearray ("));

        for i in 0..data.len() {
            if i > 0 { sb_appendf(output, c!(" ")); }
            sb_appendf(output, c!("%02X"), (*data)[i] as c_uint);
        }
        sb_appendf(output, c!(")\n"));
        sb_appendf(output, c!("}\n"));
    }
}

pub unsafe fn generate_extrn_lib_resolver(output: *mut String_Builder, lib: *const c_char, use_underscored_name: bool, mono: bool) {
    sb_appendf(output, c!("        ldsfld native int Program::'<%s_lib>'\n"), lib);
    if use_underscored_name { sb_appendf(output, c!("        ldstr \"_\"\n")); }
    sb_appendf(output, c!("        ldarg.0\n"));
    if use_underscored_name { sb_appendf(output, c!("       call string [mscorlib]System.String::Concat(string, string)\n")); }
    if !mono {
        sb_appendf(output, c!("        ldloca.s 0\n"));
        sb_appendf(output, c!("        call bool [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeLibrary::TryGetExport(native int, string, native int&)\n"));
    }
    else {
        sb_appendf(output, c!("        call native int Program::'<GetExport>'(native int, string)\n"));
        sb_appendf(output, c!("        stloc.0\n"));
        sb_appendf(output, c!("        ldloc.0\n"));
    }
    sb_appendf(output, c!("        brtrue Success\n"));
}

pub unsafe fn generate_loader_helpers(output: *mut String_Builder, linker: *const [*const c_char], mono: bool) {
    if !mono {
        sb_appendf(output, c!("    .method static bool '<TryLoadLibrary>'(string, native int&) {\n"));
        sb_appendf(output, c!("        ldarg.0\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        call bool [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeLibrary::TryLoad(string, native int&)\n"));
        sb_appendf(output, c!("        brfalse.s CurrentDir\n"));
        sb_appendf(output, c!("        ldc.i4.1\n"));
        sb_appendf(output, c!("        ret\n"));
        sb_appendf(output, c!("    CurrentDir:\n"));
        sb_appendf(output, c!("        call string [mscorlib]System.IO.Directory::GetCurrentDirectory()\n"));
        sb_appendf(output, c!("        ldarg.0\n"));
        sb_appendf(output, c!("        call string [mscorlib]System.IO.Path::Combine(string, string)\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        call bool [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeLibrary::TryLoad(string, native int&)\n"));
        sb_appendf(output, c!("        ret\n"));
        sb_appendf(output, c!("    }\n"));
    }
    else {
        sb_appendf(output, c!("    .method static pinvokeimpl(\"libc\" as \"dlopen\" nomangle ansi cdecl) native int '<dlopen>'(string, int32) preservesig {}\n"));
        sb_appendf(output, c!("    .method static pinvokeimpl(\"libc\" as \"dlsym\" nomangle ansi cdecl) native int '<dlsym>'(native int, string) preservesig {}\n"));
        sb_appendf(output, c!("    .method static pinvokeimpl(\"kernel32.dll\" as \"LoadLibraryA\" nomangle ansi winapi) native int '<LoadLibraryA>'(string) preservesig {}\n"));
        sb_appendf(output, c!("    .method static pinvokeimpl(\"kernel32.dll\" as \"GetProcAddress\" nomangle ansi winapi) native int '<GetProcAddress>'(native int, string) preservesig {}\n"));

        sb_appendf(output, c!("    .method static bool '<TryLoadLibrary>'(string, native int&) {\n"));
        sb_appendf(output, c!("        ldsfld bool Program::'<IsWindows>'\n"));
        sb_appendf(output, c!("        brfalse.s Unix\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        ldarg.0\n"));
        sb_appendf(output, c!("        call native int Program::'<LoadLibraryA>'(string)\n"));
        sb_appendf(output, c!("        stind.i\n"));
        sb_appendf(output, c!("        br.s Return\n"));
        sb_appendf(output, c!("    Unix:\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        ldarg.0\n"));
        sb_appendf(output, c!("        ldc.i4.1\n"));
        sb_appendf(output, c!("        call native int Program::'<dlopen>'(string, int32)\n"));
        sb_appendf(output, c!("        stind.i\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        ldind.i\n"));
        sb_appendf(output, c!("        brfalse.s CurrentDir\n"));
        sb_appendf(output, c!("        br.s Return\n"));
        sb_appendf(output, c!("    CurrentDir:\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        call string [mscorlib]System.IO.Directory::GetCurrentDirectory()\n"));
        sb_appendf(output, c!("        ldarg.0\n"));
        sb_appendf(output, c!("        call string [mscorlib]System.IO.Path::Combine(string, string)\n"));
        sb_appendf(output, c!("        ldc.i4.1\n"));
        sb_appendf(output, c!("        call native int Program::'<dlopen>'(string, int32)\n"));
        sb_appendf(output, c!("        stind.i\n"));
        sb_appendf(output, c!("    Return:\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        ldind.i\n"));
        sb_appendf(output, c!("        ldsfld native int [mscorlib]System.IntPtr::Zero\n"));
        sb_appendf(output, c!("        ceq\n"));
        sb_appendf(output, c!("        ldc.i4.0\n"));
        sb_appendf(output, c!("        ceq\n"));
        sb_appendf(output, c!("        ret\n"));
        sb_appendf(output, c!("    }\n"));
    }

    sb_appendf(output, c!("    .method static native int '<LoadLibrary>'(string) {\n"));
    sb_appendf(output, c!("        .locals init (native int lib)\n"));
    sb_appendf(output, c!("        ldsfld bool Program::'<IsWindows>'\n"));
    sb_appendf(output, c!("        brfalse.s Unix\n"));
    sb_appendf(output, c!("        ldarg.0\n"));
    sb_appendf(output, c!("        ldstr \".dll\"\n"));
    sb_appendf(output, c!("        call string [mscorlib]System.String::Concat(string, string)\n"));
    sb_appendf(output, c!("        ldloca.s 0\n"));
    if mono {
        sb_appendf(output, c!("        call bool Program::'<TryLoadLibrary>'(string, native int&)\n"));
    }
    else {
        sb_appendf(output, c!("        call bool [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeLibrary::TryLoad(string, native int&)\n"));
    }
    sb_appendf(output, c!("        brtrue.s Success\n"));
    sb_appendf(output, c!("        br.s Failed\n"));
    sb_appendf(output, c!("    Unix:\n"));
    sb_appendf(output, c!("        ldarg.0\n"));
    sb_appendf(output, c!("        ldsfld string Program::'<PosixSuffix>'\n"));
    sb_appendf(output, c!("        call string [mscorlib]System.String::Concat(string, string)\n"));
    sb_appendf(output, c!("        ldloca.s 0\n"));
    sb_appendf(output, c!("        call bool Program::'<TryLoadLibrary>'(string, native int&)\n"));
    sb_appendf(output, c!("        brtrue.s Success\n"));
    sb_appendf(output, c!("        ldstr \"lib\"\n"));
    sb_appendf(output, c!("        ldarg.0\n"));
    sb_appendf(output, c!("        ldsfld string Program::'<PosixSuffix>'\n"));
    sb_appendf(output, c!("        call string [mscorlib]System.String::Concat(string, string, string)\n"));
    sb_appendf(output, c!("        ldloca.s 0\n"));
    sb_appendf(output, c!("        call bool Program::'<TryLoadLibrary>'(string, native int&)\n"));
    sb_appendf(output, c!("        brtrue.s Success\n"));
    sb_appendf(output, c!("    Failed:\n"));
    sb_appendf(output, c!("        call valuetype [mscorlib]System.ConsoleColor [mscorlib]System.Console::get_ForegroundColor()\n"));
    sb_appendf(output, c!("        ldc.i4.s 14\n"));
    sb_appendf(output, c!("        call void [mscorlib]System.Console::set_ForegroundColor(valuetype [mscorlib]System.ConsoleColor)\n"));
    sb_appendf(output, c!("        ldstr \"[WARNING] Unable to load library \"\n"));
    sb_appendf(output, c!("        ldarg.0\n"));
    sb_appendf(output, c!("        call string [mscorlib]System.String::Concat(string, string)\n"));
    sb_appendf(output, c!("        call void [mscorlib]System.Console::WriteLine(string)\n"));
    sb_appendf(output, c!("        call void [mscorlib]System.Console::set_ForegroundColor(valuetype [mscorlib]System.ConsoleColor)\n"));
    sb_appendf(output, c!("        ldc.i8 -2\n"));
    sb_appendf(output, c!("        conv.i\n"));
    sb_appendf(output, c!("        ret\n"));
    sb_appendf(output, c!("    Success:\n"));
    sb_appendf(output, c!("        ldloc.0\n"));
    sb_appendf(output, c!("        ret\n"));
    sb_appendf(output, c!("    }\n"));

    if mono {
        sb_appendf(output, c!("    .method static native int '<GetExport>'(native int, string) {\n"));
        sb_appendf(output, c!("        ldarg.0\n"));
        sb_appendf(output, c!("        ldarg.1\n"));
        sb_appendf(output, c!("        ldsfld bool Program::'<IsWindows>'\n"));
        sb_appendf(output, c!("        brfalse.s Unix\n"));
        sb_appendf(output, c!("        call native int Program::'<GetProcAddress>'(native int, string)\n"));
        sb_appendf(output, c!("        ret\n"));
        sb_appendf(output, c!("    Unix:\n"));
        sb_appendf(output, c!("        call native int Program::'<dlsym>'(native int, string)\n"));
        sb_appendf(output, c!("        ret\n"));
        sb_appendf(output, c!("    }\n"));
    }

    sb_appendf(output, c!("    .method static native int '<ResolveExtrn>'(string) {\n"));
    sb_appendf(output, c!("        .locals init (native int fnptr)\n"));
    generate_extrn_lib_resolver(output, c!("libc"), false, mono);
    generate_extrn_lib_resolver(output, c!("libc"), true, mono);
    for i in 0..linker.len() {
        let lib = (*linker)[i];
        generate_extrn_lib_resolver(output, lib, false, mono);
    }
    sb_appendf(output, c!("    Failed:\n"));
    sb_appendf(output, c!("        ldstr \"Unable to resolve extrn \"\n"));
    sb_appendf(output, c!("        ldarg.0\n"));
    sb_appendf(output, c!("        call string [mscorlib]System.String::Concat(string, string)\n"));
    sb_appendf(output, c!("        newobj instance void [mscorlib]System.Exception::.ctor(string)\n"));
    sb_appendf(output, c!("        throw\n"));
    sb_appendf(output, c!("    Success:\n"));
    sb_appendf(output, c!("        ldloc.0\n"));
    sb_appendf(output, c!("        ret\n"));
    sb_appendf(output, c!("    }\n"));
}

pub unsafe fn generate_constructor(output: *mut String_Builder, globals: *const [Global], linker: *const [*const c_char], mono: bool, has_variadics: bool, undefined_extrns: *const [*const c_char]) {
    sb_appendf(output, c!("    .method static void .cctor() {\n"));

    if undefined_extrns.len() > 0 {
        sb_appendf(output, c!("        call valuetype [mscorlib]System.Runtime.InteropServices.OSPlatform [mscorlib]System.Runtime.InteropServices.OSPlatform::get_Windows()\n"));
        sb_appendf(output, c!("        call bool [mscorlib]System.Runtime.InteropServices.RuntimeInformation::IsOSPlatform(valuetype [mscorlib]System.Runtime.InteropServices.OSPlatform)\n"));
        sb_appendf(output, c!("        stsfld bool Program::'<IsWindows>'\n"));
        sb_appendf(output, c!("        call valuetype [mscorlib]System.Runtime.InteropServices.OSPlatform [mscorlib]System.Runtime.InteropServices.OSPlatform::get_Linux()\n"));
        sb_appendf(output, c!("        call bool [mscorlib]System.Runtime.InteropServices.RuntimeInformation::IsOSPlatform(valuetype [mscorlib]System.Runtime.InteropServices.OSPlatform)\n"));
        sb_appendf(output, c!("        stsfld bool Program::'<IsLinux>'\n"));
        if has_variadics {
            sb_appendf(output, c!("        call valuetype [mscorlib]System.Runtime.InteropServices.OSPlatform [mscorlib]System.Runtime.InteropServices.OSPlatform::get_OSX()\n"));
            sb_appendf(output, c!("        call bool [mscorlib]System.Runtime.InteropServices.RuntimeInformation::IsOSPlatform(valuetype [mscorlib]System.Runtime.InteropServices.OSPlatform)\n"));
            sb_appendf(output, c!("        call valuetype [mscorlib]System.Runtime.InteropServices.Architecture [mscorlib]System.Runtime.InteropServices.RuntimeInformation::get_ProcessArchitecture()\n"));
            sb_appendf(output, c!("        ldc.i4.3\n"));
            sb_appendf(output, c!("        ceq\n"));
            sb_appendf(output, c!("        and\n"));
            sb_appendf(output, c!("        stsfld bool Program::'<IsMacOS_ARM64>'\n"));
        }

        sb_appendf(output, c!("        ldsfld bool Program::'<IsLinux>'\n"));
        sb_appendf(output, c!("        brfalse.s macOS\n"));
        sb_appendf(output, c!("        ldstr \".so\"\n"));
        sb_appendf(output, c!("        br.s SetSuffix\n"));
        sb_appendf(output, c!("    macOS:\n"));
        sb_appendf(output, c!("        ldstr \".dylib\"\n"));
        sb_appendf(output, c!("    SetSuffix:\n"));
        sb_appendf(output, c!("        stsfld string Program::'<PosixSuffix>'\n"));

        sb_appendf(output, c!("        ldsfld bool Program::'<IsWindows>'\n"));
        sb_appendf(output, c!("        brfalse.s libc\n"));
        sb_appendf(output, c!("        ldstr \"msvcrt\"\n"));
        sb_appendf(output, c!("        call native int Program::'<LoadLibrary>'(string)\n"));
        sb_appendf(output, c!("        br.s set_libc\n"));
        sb_appendf(output, c!("    libc:\n"));
        if mono {
            sb_appendf(output, c!("        ldnull\n"));
            sb_appendf(output, c!("        ldc.i4.1\n"));
            sb_appendf(output, c!("        call native int Program::'<dlopen>'(string, int32)\n"));
        }
        else {
            sb_appendf(output, c!("        call native int [System.Runtime.InteropServices]System.Runtime.InteropServices.NativeLibrary::GetMainProgramHandle()\n"));
        }
        sb_appendf(output, c!("    set_libc:\n"));
        sb_appendf(output, c!("        stsfld native int Program::'<libc_lib>'\n"));

        for i in 0..linker.len() {
            let lib = (*linker)[i];
            sb_appendf(output, c!("        ldstr \"%s\"\n"), lib);
            sb_appendf(output, c!("        call native int Program::'<LoadLibrary>'(string)\n"));
            sb_appendf(output, c!("        stsfld native int Program::'<%s_lib>'\n"), lib);
        }

        for i in 0..undefined_extrns.len() {
            let extrn = (*undefined_extrns)[i];
            sb_appendf(output, c!("        ldstr \"%s\"\n"), extrn);
            sb_appendf(output, c!("        call native int Program::'<ResolveExtrn>'(string)\n"));
            sb_appendf(output, c!("        stsfld native int Program::'<%s_fnptr>'\n"), extrn);
        }
    }

    for i in 0..globals.len() {
        let global = (*globals)[i];
        let is_array = global.values.count > 1;
        if is_array {
            sb_appendf(output, c!("        ldc.i8 %zd\n"), global.values.count * 8);
            sb_appendf(output, c!("        ldsfld native int Program::'<malloc_fnptr>'\n"));
            sb_appendf(output, c!("        calli unmanaged cdecl int64(int64)"));
            sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name);
        }

        for j in 0..global.values.count {
            match *global.values.items.add(j) {
                ImmediateValue::Literal(lit) => {
                    if !is_array {
                        sb_appendf(output, c!("        ldc.i8 %zd\n"), lit);
                        sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name)
                    } else {
                        sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), global.name);
                        sb_appendf(output, c!("        ldc.i8 %zd\n"), j * 8);
                        sb_appendf(output, c!("        add\n"));
                        sb_appendf(output, c!("        ldc.i8 %zd\n"), lit);
                        sb_appendf(output, c!("        stind.i8\n"))
                    }
                },
                ImmediateValue::Name(name) => {
                    if !is_array {
                        sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), name);
                        sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name)
                    } else {
                        sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), global.name);
                        sb_appendf(output, c!("        ldc.i8 %zd\n"), j * 8);
                        sb_appendf(output, c!("        add\n"));
                        sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), name);
                        sb_appendf(output, c!("        stind.i8\n"))
                    }
                }
                ImmediateValue::DataOffset(offset) => {
                    if !is_array {
                        sb_appendf(output, c!("        ldsflda valuetype '<BLangDataSection>'/'DataSection' '<BLangDataSection>'::'Data'\n"));
                        sb_appendf(output, c!("        ldc.i8 %zd\n"), offset);
                        sb_appendf(output, c!("        add\n"));
                        sb_appendf(output, c!("        stsfld int64 Program::'%s'\n"), global.name)
                    } else {
                        sb_appendf(output, c!("        ldsfld int64 Program::'%s'\n"), global.name);
                        sb_appendf(output, c!("        ldc.i8 %zd\n"), j * 8);
                        sb_appendf(output, c!("        add\n"));
                        sb_appendf(output, c!("        ldsflda valuetype '<BLangDataSection>'/'DataSection' '<BLangDataSection>'::'Data'\n"));
                        sb_appendf(output, c!("        ldc.i8 %zd\n"), offset);
                        sb_appendf(output, c!("        add\n"));
                        sb_appendf(output, c!("        stind.i8\n"))
                    }
                },
            };
        }
    }

    sb_appendf(output, c!("        ret\n"));
    sb_appendf(output, c!("    }\n"));
}

pub unsafe fn generate_fields(output: *mut String_Builder, globals: *const [Global], extrns: *const [*const c_char], funcs: *const [Func], linker: *const [*const c_char], mono: bool, has_variadics: bool) {
    for i in 0..globals.len() {
        sb_appendf(output, c!("    .field public static int64 '%s'\n"), (*globals)[i].name);
    }

    let mut has_malloc = false;
    let mut undefined_extrns: Array<*const c_char> = zeroed();

    for i in 0..extrns.len() {
        let extrn = (*extrns)[i];

        if strcmp(extrn, c!("malloc")) == 0 {
            has_malloc = true;
        }

        let mut extrn_defined = false;
        for j in 0..funcs.len() {
            let func = (*funcs)[j];
            if strcmp(func.name, extrn) == 0 {
                extrn_defined = true;
                break;
            }
        }

        if !extrn_defined {
            da_append(&mut undefined_extrns, extrn);
            sb_appendf(output, c!("    .field public static native int '<%s_fnptr>'\n"), extrn);
        }
    }

    // we need malloc for initializing global arrays
    if !has_malloc && globals.len() > 0 {
        da_append(&mut undefined_extrns, c!("malloc"));
        sb_appendf(output, c!("    .field public static native int '<malloc_fnptr>'\n"));
    }

    sb_appendf(output, c!("    .field public static native int '<libc_lib>'\n"));

    let has_undefined_extrns = undefined_extrns.count > 0;
    for i in 0..linker.len() {
        let lib = (*linker)[i];
        sb_appendf(output, c!("    .field public static native int '<%s_lib>'\n"), lib);
    }

    if globals.len() > 0 || has_undefined_extrns {
        if has_undefined_extrns {
            sb_appendf(output, c!("    .field public static string '<PosixSuffix>'\n"));

            sb_appendf(output, c!("    .field public static bool '<IsWindows>'\n"));
            sb_appendf(output, c!("    .field public static bool '<IsLinux>'\n"));
            if has_variadics {
                sb_appendf(output, c!("    .field public static bool '<IsMacOS_ARM64>'\n"));
            }

            generate_loader_helpers(output, linker, mono);
        }

        generate_constructor(output, globals, linker, mono, has_variadics, da_slice(undefined_extrns));
    }
}

pub unsafe fn usage(params: *const [Param], mono: bool) {
    fprintf(stderr(), c!("%s codegen for the B compiler\n"), if mono { c!("ilasm_mono") } else { c!("ilasm_core") });
    fprintf(stderr(), c!("OPTIONS:\n"));
    print_params_help(params);
}

struct ILasm {
    link_args: *const c_char,
    output: String_Builder,
    cmd: Cmd,
    mono: bool,
}

pub unsafe fn get_apis(targets: *mut Array<TargetAPI>) {
    da_append(targets, TargetAPI::V1 {
        name: c!("ilasm-mono"),
        file_ext: c!(".exe"),
        new: |a, args| {
            new(a, args, true)
        },
        build: generate_program,
        run: run_program,
    });

    da_append(targets, TargetAPI::V1 {
        name: c!("ilasm-core"),
        file_ext: c!(".b.dll"),
        new: |a, args| {
            new(a, args, false)
        },
        build: generate_program,
        run: run_program,
    });
}

pub unsafe fn new(a: *mut arena::Arena, args: *const [*const c_char], mono: bool) -> Option<*mut c_void> {
    let gen = arena::alloc_type::<ILasm>(a);
    memset(gen as _ , 0, size_of::<ILasm>());
    (*gen).mono = mono;

    let mut help = false;
    let params = &[
        Param {
            name:        c!("help"),
            description: c!("Print this help message"),
            value:       ParamValue::Flag { var: &mut help },
        },
        Param {
            // This is called "link-args" to remain compatible with the old -L syntax which is automatically turned into -C link-args='...' by the B compiler
            name:        c!("link-args"),
            description: c!("List of native dynamic libraries (without file extension nor the 'lib' prefix) to search for external functions in, Example: -C link-args='raylib'"),
            value:       ParamValue::String { var: &mut (*gen).link_args, default: c!("") },
        },
    ];

    if let Err(message) = parse_args(params, args) {
        usage(params, mono);
        log(Log_Level::ERROR, c!("%s"), message);
        return None;
    }

    if help {
        usage(params, mono);
        return None;
    }
    Some(gen as _)
}

pub unsafe fn generate_program(
    gen: *mut c_void, program: *const Program, program_path: *const c_char, garbage_base: *const c_char,
    _nostdlib: bool, debug: bool,
) -> Option<()> {
    let gen = gen as *mut ILasm;
    let output = &mut (*gen).output;
    let cmd = &mut (*gen).cmd;
    let mono = (*gen).mono;

    if debug { todo!("Debug information for ilasm") }

    sb_appendf(output, c!(".assembly 'Main' {}\n"));
    sb_appendf(output, c!(".assembly extern mscorlib {}\n"));
    if !mono {
        sb_appendf(output, c!(".assembly extern System.Runtime {}\n"));
        sb_appendf(output, c!(".assembly extern System.Runtime.InteropServices {}\n"));
    }
    sb_appendf(output, c!(".module Main.%s\n"), if mono { c!("exe") } else { c!("dll") });

    let sliced_data = da_slice((*program).data);
    generate_data_section(output, sliced_data);

    sb_appendf(output, c!(".class Program extends [mscorlib]System.Object {\n"));

    let mut libs: Array<*const c_char> = zeroed();

    let mut s: Shlex = zeroed();
    let link_args = (*gen).link_args;
    shlex_init(&mut s, link_args, link_args.add(strlen(link_args)));
    while !shlex_next(&mut s).is_null() {
        da_append(&mut libs, temp_strdup(s.string));
    }
    shlex_free(&mut s);

    let linker = da_slice(libs);
    let funcs = da_slice((*program).funcs);
    let variadics = da_slice((*program).variadics);
    generate_fields(output, da_slice((*program).globals), da_slice((*program).extrns), funcs, linker, mono, variadics.len() > 0);
    generate_funcs(funcs, output, sliced_data, variadics);

    sb_appendf(output, c!("    .method static void Main (string[] args) {\n"));
    sb_appendf(output, c!("        .entrypoint\n"));
    sb_appendf(output, c!("        call int64 class Program::main()\n"));
    sb_appendf(output, c!("        pop\n"));
    sb_appendf(output, c!("        ret\n"));
    sb_appendf(output, c!("    }\n"));
    sb_appendf(output, c!("}\n"));

    let output_asm_path = temp_sprintf(c!("%s.il"), garbage_base);
    write_entire_file(output_asm_path, (*output).items as *const c_void, (*output).count)?;
    log(Log_Level::INFO, c!("generated %s"), output_asm_path);

    if mono {
        cmd_append! {
            cmd,
            c!("ilasm"), output_asm_path, temp_sprintf(c!("-output:%s"), program_path)
        }
    }
    else {
        cmd_append! {
            cmd,
            c!("ilasm"), c!("-dll"), output_asm_path, temp_sprintf(c!("-output:%s"), program_path)
        }
    }

    if !cmd_run_sync_and_reset(cmd) { return None; }

    if !mono {
        let base_path;
        if let Some(path) = temp_strip_suffix(program_path, c!(".dll")) {
            base_path = path;
        } else {
            base_path = program_path;
        }

        let config_output_path = temp_sprintf(c!("%s.runtimeconfig.json"), base_path);
        let config = c!("
        {
            \"runtimeOptions\": {
                \"tfm\": \"net9.0\",
                \"framework\": {
                    \"name\": \"Microsoft.NETCore.App\",
                    \"version\": \"9.0.0\"
                }
            }
        }");

        write_entire_file(config_output_path, config as *const c_void, strlen(config))?;
    }

    Some(())
}

pub unsafe fn run_program(
    gen: *mut c_void, program_path: *const c_char, run_args: *const [*const c_char],
) -> Option<()> {
    let gen = gen as *mut ILasm;
    let cmd = &mut (*gen).cmd;
    let mono = (*gen).mono;

    if mono && !cfg!(target_os = "windows") {
        cmd_append! {
            cmd,
            c!("mono"),
        }
    }

    if !mono {
        cmd_append! {
            cmd,
            c!("dotnet"),
        }
    }

    cmd_append!{ cmd, program_path, }

    da_append_many(cmd, run_args);
    if !cmd_run_sync_and_reset(cmd) { return None; }
    Some(())
}

// TODO: make this codegen self-contained eventually.
//   That is generate .exe directly without the help of ilasm.
