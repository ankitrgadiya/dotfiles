# Defined in /tmp/fish.BvIMmT/c.fish @ line 2
function c
    set tmpdir "$HOME/.tmp"
    set cols (math "floor($COLUMNS/3)")
    set sep '{::}'
    switch (uname)
        case Linux
            set google_history "$HOME/.config/google-chrome/Default/History"
            set open "xdg-open"
        case Darwin
            set google_history "$HOME/Library/Application Support/Google/Chrome/Default/History"
            set open "open"
    end

    # Ensure the temp directory is present.
    mkdir -p "$tmpdir"

    yes | "cp" -f "$google_history" "$tmpdir/chrome-history"
    sqlite3 -separator $sep "$tmpdir/chrome-history" \
        "select substr(title, 1, $cols), url
        from urls order by last_visit_time desc" |
    awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
    fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs $open >/dev/null 2>/dev/null
end
