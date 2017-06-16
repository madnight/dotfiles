case "$1" in
right)  p=215 && _(){ bspc config right_padding "$@" } && [[ $(_) -eq $p ]] && _ 0 || _ $p
    ;;
left)  p=410 && _(){ bspc config left_padding "$@" } && [[ $(_) -eq $p ]] && _ 0 || _ $p
    ;;
down)  p=220 && _(){ bspc config bottom_padding "$@" } && [[ $(_) -eq $p ]] && _ 0 || _ $p
    ;;
esac
