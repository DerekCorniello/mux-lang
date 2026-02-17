use std::env;
use std::path::{Path, PathBuf};

fn main() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .expect("mux-compiler should have a parent directory");

    // Set up git hooks path for development
    if env::var("CI").is_err() {
        let _ = set_git_hooks_dir::setup(workspace_root.join(".github/hooks"), workspace_root);
    }

    let profile = if env::var("CARGO_PROFILE_RELEASE").is_ok() {
        "release"
    } else {
        "debug"
    };

    let target_dir = env::var("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| workspace_root.join("target"));

    let debug_path = target_dir.join("debug");
    let release_path = target_dir.join("release");
    let profile_path = target_dir.join(profile);

    let (static_lib, dynamic_lib) =
        detect_runtime_library(workspace_root, &debug_path, &release_path, &profile_path);

    println!(
        "cargo:rustc-env=MUX_RUNTIME_STATIC={}",
        static_lib.display()
    );
    println!(
        "cargo:rustc-env=MUX_RUNTIME_DYNAMIC={}",
        dynamic_lib.display()
    );
    println!("cargo:rustc-env=MUX_RUNTIME_DIR={}", profile_path.display());
}

#[cfg(target_family = "unix")]
fn detect_runtime_library(
    workspace_root: &Path,
    debug_path: &Path,
    release_path: &Path,
    profile_path: &Path,
) -> (PathBuf, PathBuf) {
    let candidates: Vec<(PathBuf, &str)> = vec![
        (
            profile_path.to_path_buf(),
            "profile-specific (release or debug)",
        ),
        (release_path.to_path_buf(), "release"),
        (debug_path.to_path_buf(), "debug"),
        (
            workspace_root.join("target").join("release"),
            "workspace/target/release",
        ),
        (
            workspace_root.join("target").join("debug"),
            "workspace/target/debug",
        ),
    ];

    for (path, description) in &candidates {
        let static_lib = path.join("libmux_runtime.a");
        let dynamic_lib = path.join("libmux_runtime.so");
        if static_lib.exists() || dynamic_lib.exists() {
            println!(
                "cargo:warning=Found libmux_runtime in {} (checking {})",
                path.display(),
                description
            );
            return (static_lib, dynamic_lib);
        }
    }

    let static_lib = profile_path.join("libmux_runtime.a");
    let dynamic_lib = profile_path.join("libmux_runtime.so");

    if !static_lib.exists() && !dynamic_lib.exists() {
        println!(
            "cargo:warning=Could not find libmux_runtime yet. Expected locations:\n\
             - {} (profile-specific)\n\
             - {}\n\
             - {}\n\
             - {}\n\
             - {}\n\
             The mux CLI will request a runtime build if needed.",
            profile_path.display(),
            release_path.display(),
            debug_path.display(),
            workspace_root.join("target").join("release").display(),
            workspace_root.join("target").join("debug").display()
        );
    }

    (static_lib, dynamic_lib)
}

#[cfg(target_family = "windows")]
fn detect_runtime_library(
    workspace_root: &Path,
    debug_path: &Path,
    release_path: &Path,
    profile_path: &Path,
) -> (PathBuf, PathBuf) {
    let check_path = |p: &Path| -> (PathBuf, PathBuf) {
        let static_lib = p.join("mux_runtime.lib");
        let dynamic_lib = p.join("mux_runtime.dll");
        if static_lib.exists() {
            (static_lib, dynamic_lib)
        } else if dynamic_lib.exists() {
            (static_lib, dynamic_lib)
        } else {
            (static_lib, dynamic_lib)
        }
    };

    let candidates: Vec<(PathBuf, &str)> = vec![
        (
            profile_path.to_path_buf(),
            "profile-specific (release or debug)",
        ),
        (release_path.to_path_buf(), "release"),
        (debug_path.to_path_buf(), "debug"),
        (
            workspace_root.join("target").join("release"),
            "workspace/target/release",
        ),
        (
            workspace_root.join("target").join("debug"),
            "workspace/target/debug",
        ),
    ];

    for (path, description) in &candidates {
        let (static_lib, dynamic_lib) = check_path(path);
        if static_lib.exists() || dynamic_lib.exists() {
            println!(
                "cargo:warning=Found mux_runtime in {} (checking {})",
                path.display(),
                description
            );
            return (static_lib, dynamic_lib);
        }
    }

    let static_lib = profile_path.join("mux_runtime.lib");
    let dynamic_lib = profile_path.join("mux_runtime.dll");

    if !static_lib.exists() && !dynamic_lib.exists() {
        println!(
            "cargo:warning=Could not find mux_runtime yet. Expected locations:\n\
             - {} (profile-specific)\n\
             - {}\n\
             - {}\n\
             - {}\n\
             - {}\n\
             The mux CLI will request a runtime build if needed.",
            profile_path.display(),
            release_path.display(),
            debug_path.display(),
            workspace_root.join("target").join("release").display(),
            workspace_root.join("target").join("debug").display()
        );
    }

    (static_lib, dynamic_lib)
}
