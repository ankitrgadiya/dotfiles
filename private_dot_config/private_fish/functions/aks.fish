function aks
	switch $argv[1]
	case "stern"
		KUBECONFIG=$HOME/.kube/aks-config stern $argv[2..]
	case "kubens"
		KUBECONFIG=$HOME/.kube/aks-config kubens $argv[2..]
	case "k9s"
		KUBECONFIG=$HOME/.kube/aks-config k9s $argv[2..]
	case "*"
		KUBECONFIG=$HOME/.kube/aks-config kubectl $argv[1..]
	end
end
