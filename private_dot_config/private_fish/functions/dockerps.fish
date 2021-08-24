function dockerps
	docker ps --format '"{{.Status}}" "{{.ID}}" "{{.Names}}"' | awk -F '"' '{printf("%20s| %-10s | %-10s\n", $2, $4, substr($6, 1, 30)"...")}'
end
