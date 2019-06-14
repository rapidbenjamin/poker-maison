type username = string;

type userProfile = {
  proUsername: username,
  proEmail: string,
  proAvailableChips: int,
  proChipsInPlay: int,
  proUserCreated: int,
};

type err = string;

type state =
  | NotLoading
  | Loading
  | Error(err)
  | Loaded(userProfile);

type action =
  | ProfileFetch
  | ProfileFetched(userProfile)
  | ProfileFailedToFetch(err);

let reducer = (_prevState, action) =>
  switch (action) {
  | ProfileFetch => Loading
  | ProfileFetched(userProfile) => Loaded(userProfile)
  | ProfileFailedToFetch(err) => Error(err)
  };

let initialState = NotLoading;

let getProfile = () =>
  Js.Promise.(
    Fetch.fetchWithInit(
      "/api/hello",
      Fetch.RequestInit.make(
        ~headers=Fetch.HeadersInit.make({"auth": "faketoken"}),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
  );

[@react.component]
let make = (~className) => {
  let (state, dispatch) = React.useReducer(reducer, initialState);

  React.useEffect(() => {
    //fetchData;
    dispatch(ProfileFetch);
    Js.log("fetching");
    None;
  });

  switch (state) {
  | _ =>
    <div>
      <p> {React.string(" get your profile")} </p>
      <button onClick={_ => Js.log("clicked")}>
        {React.string("Click me")}
      </button>
    </div>
  };
};