module type RouterConfig = {
  type route;
  let routeFromUrl: ReasonReact.Router.url => route;
  let routeToUrl: route => string;
};

module CreateRouter = (Config: RouterConfig) => {
  module Container = {
    type action =
      | ChangeRoute(Config.route);
    type state = {
      route: Config.route,
      watchUrlId: ref(ReasonReact.Router.watcherID),
    };
    let component = ReasonReact.reducerComponent("CallstackRerouteRouter");
    let make = children => {
      ...component,
      initialState: () =>
        ReasonReact.Router.dangerouslyGetInitialUrl() |> Config.routeFromUrl,
      reducer: (action, state) =>
        switch (action) {
        | ChangeRoute(route) => ReasonReact.Update({...state, route})
        },
      didMount: self =>
        self.state.watchUrlId :=
          ReasonReact.Router.watchUrl(url =>
            self.send(ChangeRoute(url |> Config.routeFromUrl))
          ),
      willUnmount: self =>
        ReasonReact.Router.unwatchUrl(self.state.watchUrlId^),
      render: self => children(~currentRoute=self.state.route),
    };
  };
  module Link = {
    let component = ReasonReact.statelessComponent("CallstackRerouteLink");
    let make = (~route, children) => {
      ...component,
      render: _self => {
        let href = Config.routeToUrl(route);
        <a
          href
          onClick=(
            event => {
              ReactEventRe.Synthetic.preventDefault(event);
              ReasonReact.Router.push(href);
            }
          )>
          (ReasonReact.array(children))
        </a>;
      },
    };
  };
};