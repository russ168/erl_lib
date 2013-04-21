{application, services, 
[{description, "Services"},
 {vsn, "1.0"},
 {modules, [services_app, services_supervisor,
            pdc]},
 {registered, [pdc]},
 {applications, [kernel, stdlib]},
 {mod, {services_app, []}}, 
 {start_phases, []}
]}.
 

	    