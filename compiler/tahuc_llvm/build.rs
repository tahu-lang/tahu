use std::process::Command;

fn get_lib_path() -> String {
    let output = Command::new("llvm-config")
        .arg("--libdir")
        .output()
        .expect("Failed to execute llvm_config");

    String::from_utf8_lossy(&output.stdout).to_string()
}

fn main() {
    let lib_path = get_lib_path();

    // search lib
    println!("cargo:rustc-link-search=native={}", lib_path.trim());

    let libs = String::from_utf8(
        Command::new("llvm-config")
            .args(&["--libnames", "core", "analysis", "target", "codegen", "object"])
            .args(&["x86", "arm", "aarch64", "amdgpu", "loongarch", "mips", "WebAssembly"])
            .output()
            .expect("failed to run llvm-config --libs")
            .stdout,
    )
    .unwrap();

    for lib in libs.split_whitespace() {
        let mut name = lib.to_string();

        if cfg!(target_os = "windows") {
            // LLVMCore.lib -> LLVMCore
            if let Some(stem) = std::path::Path::new(&name).file_stem() {
                name = stem.to_str().unwrap().to_string();
            }
            println!("cargo:rustc-link-lib=static={}", name);
        } else {
            // libLLVM-18.so -> LLVM-18
            if name.starts_with("lib") {
                name = name.trim_start_matches("lib").to_string();
            }
            if let Some(stripped) = name.split('.').next() {
                name = stripped.to_string();
            }
            println!("cargo:rustc-link-lib=dylib={}", name);
        }
    }
}
