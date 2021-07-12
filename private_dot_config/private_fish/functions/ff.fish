# Defined in /tmp/fish.BvIMmT/c.fish @ line 2
function ff
    set tmpdir "$HOME/.tmp"
    set cols (math "floor($COLUMNS/3)")
    set sep '{::}'
    switch (uname)
        case Linux
            set firefox_history "$HOME/.mozilla/firefox/default/places.sqlite"
            set open "xdg-open"
    end

    # Ensure the temp directory is present.
    mkdir -p "$tmpdir"

    yes | "cp" -f "$firefox_history" "$tmpdir/firefox-history"
    sqlite3 -separator $sep "$tmpdir/firefox-history" \
        "select substr(title, 1, $cols), url
        from moz_places
        where url not like '%google%search'
        order by visit_count desc,
        last_visit_date desc;" |
    awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
    fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs $open >/dev/null 2>/dev/null
end
