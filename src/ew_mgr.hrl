% domain field means, that ew_mgr may consist of several web_routes,
% that belongs to the same domain (but may be on different ip/ports),
% so, domain needed to get stat information by same domain

-record(web_route, {host,                     	% Requested Host, List (Host header)
		    port,			% Requested Port, Integer (usually 80)
		    proxy_host, 		% Proxy host, List -- real hostname or IP
		    proxy_port,  		% Proxy host port, Integer
		    domain,			% Domain, that represented by this route
		    first_request_date=[],	%
		    now_seconds,		% value of seconds for calculate rps
		    requests=0,			% current value of request by last second
		    requests_total=0,		% total value of requests
		    rps_max=1000}).		% Maximum reques per seconds rate
