function de -a name c
    if test -n c
        set c fish
    end
    docker exec -it $name $c
end
