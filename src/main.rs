use std::{
    env::home_dir,
    fs::{create_dir, create_dir_all, rename},
    io::ErrorKind,
    os::unix::fs::symlink,
    path::{Path, PathBuf},
    process::exit,
};
use tracing::{error, info, warn};

fn main() {
    tracing_subscriber::fmt::init();

    // list of files/folders to symlink in homedir
    let files = vec![
        ".bashrc",
        ".bash_profile",
        ".emacs",
        ".gitignore",
        ".agignore",
        ".config/awesome/rc.lua",
        ".config/git/config_shared",
        ".config/ghostty/config",
        "git-prompt.sh",
        ".inputrc",
        ".eslintrc",
        ".jjconfig.toml",
        ".wezterm.lua",
    ]
    .into_iter()
    .map(PathBuf::from);

    let home_dir = home_dir().unwrap();
    let backup_dir = home_dir.join("dotfiles_old");
    let dotfiles_dir = home_dir.join("dotfiles");
    create_backup_dir(&backup_dir);

    if !files
        .clone()
        .map(|file| move_file_to_backup_directory(&home_dir, &backup_dir, &file))
        .all(|r| r.is_ok())
    {
        error!("Did not successfully back up some files. Stopping");
        exit(-1);
    }

    for file in files {
        info!("Linking file {:?}", file);
        link_file_to_dotfiles_directory(&home_dir, &dotfiles_dir, &file).expect("a")
    }
}

/// Creates the backup directory where old dotfiles will be moved to. If the directory already exists, it will not be
/// created again.
fn create_backup_dir(backup_dir: &PathBuf) {
    if let Err(err) = create_dir(backup_dir) {
        info!("Could not create {:#?} due to {:?}.", backup_dir, err);
    }
}

/// Move an existing file to the backup directory
fn move_file_to_backup_directory(
    home_dir: &Path,
    backup_dir: &PathBuf,
    filename: &PathBuf,
) -> std::io::Result<()> {
    if let Err(err) = rename(home_dir.join(filename), backup_dir.join(filename)) {
        if let Some(parent) = backup_dir.join(filename).parent() {
            info!("Creating parent folder {:?}", parent);
            if let Err(err) = create_dir_all(parent) {
                warn!(
                    "Could not create parent directory {:?} for {:?}",
                    parent, filename
                )
            }
        }

        if err.kind() == ErrorKind::NotFound {
            info!("Not found {:?}", home_dir.join(filename));
            return Ok(());
        }

        error!(
            "Could not move {:?} to {:?} due to {:?}.",
            filename, backup_dir, err
        );
        Err(err)
    } else {
        Ok(())
    }
}

/// Creates a hard link between the existing file and the dotfiles
fn link_file_to_dotfiles_directory(
    home_dir: &Path,
    dotfiles_dir: &Path,
    filename: &PathBuf,
) -> anyhow::Result<()> {
    symlink(dotfiles_dir.join(filename), home_dir.join(filename))?;
    Ok(())
}
