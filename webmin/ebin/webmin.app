{application, webmin, 
[{description, "Web management interface"},
 {vsn, "1.0"},
 {modules, [webmin_app, webmin_supervisor,
            webmin_httpd]},
 {registered, [webmin_httpd]},
 {applications, [kernel, stdlib]},
 {mod, {webmin_app, []}}, 
 {start_phases, []}
]}.
 

	    