{application, oma, 
[{description, "Utilities Tools"},
 {vsn, "1.0"},
 {modules, [oma_app, oma_supervisor,
            oma_code, oma_config]},
 {registered, []},
 {applications, [kernel, stdlib]},
 {mod, {oma_app, []}}, 
 {start_phases, []}
]}.
 

	    