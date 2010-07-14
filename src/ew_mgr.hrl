-record(web_route, {host,                     	% Requested Host, List (Host header)
		    port,			% Requested Port, Integer (usually 80)
		    proxy_host, 		% Proxy host, List -- real hostname or IP
		    proxy_port,  		% Proxy host port, Integer
		    requests_per_second=1000}).	% Reques per seconds rate
