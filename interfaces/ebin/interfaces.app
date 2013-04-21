{application, interfaces, 
[{description, "Receive/Send messages from/to out world"},
 {vsn, "1.0"},
 {modules, [interfaces_app, interfaces_supervisor,
            liaoliao_interface,
	    jinshiyan_interface,
	    udp_jinq]},
 {registered, []},
 {applications, [kernel, stdlib]},
 {mod, {interfaces_app, []}}, 
 {start_phases, []}
]}.
 

	    