function delete_deployment_namespace -a "project" -d "Delete all the deployment namespaces for the given Project GUID"
    if test -z "$project"
        echo "Project name is required"
        return 1
    end
    for ns in (command kubectl get pods --all-namespaces --selector=rapyuta.io/owningEntity=$project | awk '{print $1}' | uniq | tail +2)
        echo ns
    end
end
