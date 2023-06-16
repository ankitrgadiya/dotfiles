function require --argument name
    set path (command find $FISH_LIBRARY_PATH -name "$name.fish" \
                      | grep . || exit "$name not found")

    source $path
end
