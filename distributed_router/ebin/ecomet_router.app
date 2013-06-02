{application,ecomet_router,
             [{description,"ecomet router"},
              {vsn,"0.1"},
              {modules,[ecomet_offline,ecomet_router,ecomet_router_app,
                        ecomet_router_sup,ecomet_subsmanager]},
              {registered,[]},
              {mod,{ecomet_router_app,[]}},
              {env,[]},
              {applications,[kernel,stdlib]}]}.
