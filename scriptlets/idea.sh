function open-file-in-intellij-idea() {
    if [[ $(snap list | grep intellij-idea-community | wc -l) -ge 1 ]]; then
        snap run intellij-idea-community --line $1 $2
    elif [[ $(flatpak list | grep com.jetbrains.IntelliJ-IDEA-Community | wc -l) -ge 1 ]]; then
        flatpak run com.jetbrains.IntelliJ-IDEA-Community --line $1 $2
    fi
}

function open-file-in-intellij-idea-ultimate() {
    if [[ $(snap list | grep intellij-idea-ultimate | wc -l) -ge 1 ]]; then
        snap run intellij-idea-ultimate --line $1 $2
    elif [[ $(flatpak list | grep com.jetbrains.IntelliJ-IDEA-Ultimate | wc -l) -ge 1 ]]; then
        flatpak run com.jetbrains.IntelliJ-IDEA-Ultimate --line $1 $2
    fi
}
