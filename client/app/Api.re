type username =
  | Username(string);

let rec decodeUsername = json =>
  Username(json |> Json.Decode.field("arg0", Json.Decode.string));

let rec encodeUsername = (x: username) =>
  switch (x) {
  | Username(y0) => Json.Encode.string(y0)
  };

type login = {
  loginUsername: string,
  loginPassword: string,
};

let rec encodeLogin = (x: login) =>
  Json.Encode.object_([
    ("loginUsername", Json.Encode.string(x.loginUsername)),
    ("loginPassword", Json.Encode.string(x.loginPassword)),
  ]);

type register = {
  newUserEmail: string,
  newUsername: username,
  newUserPassword: string,
};

let rec encodeRegister = (x: register) =>
  Json.Encode.object_([
    ("newUserEmail", Json.Encode.string(x.newUserEmail)),
    ("newUsername", encodeUsername(x.newUsername)),
    ("newUserPassword", Json.Encode.string(x.newUserPassword)),
  ]);

type returnToken = {
  access_token: string,
  refresh_token: string,
  expiration: int,
};

let rec decodeReturnToken = json => {
  access_token: json |> Json.Decode.field("access_token", Json.Decode.string),
  refresh_token:
    json |> Json.Decode.field("refresh_token", Json.Decode.string),
  expiration: json |> Json.Decode.field("expiration", Json.Decode.int),
};

type userProfile = {
  proUsername: username,
  proEmail: string,
  proAvailableChips: int,
  proChipsInPlay: int,
  proUserCreatedAt: Js.Date.t,
};

let rec decodeUserProfile = json => {
  proUsername: json |> Json.Decode.field("proUsername", decodeUsername),
  proEmail: json |> Json.Decode.field("proEmail", Json.Decode.string),
  proAvailableChips:
    json |> Json.Decode.field("proAvailableChips", Json.Decode.int),
  proChipsInPlay:
    json |> Json.Decode.field("proChipsInPlay", Json.Decode.int),
  proUserCreatedAt:
    json |> Json.Decode.field("proUserCreatedAt", Json.Decode.date),
};

let getProfile = header_Authorization => {
  Js.Promise.(
    Fetch.fetchWithInit(
      String.concat("/", ["", "profile"]),
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=
          Fetch.HeadersInit.makeWithDict(
            Js.Dict.fromList(
              Belt.List.keepMap(
                [
                  Belt.Option.map(Some(header_Authorization), x =>
                    ("Authorization", x |> (x => x))
                  ),
                ],
                x =>
                x
              ),
            ),
          ),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
    |> then_(response =>
         response |> decodeUserProfile |> (x => Belt_Result.Ok(x)) |> resolve
       )
  );
};

let postLogin = body => {
  Js.Promise.(
    Fetch.fetchWithInit(
      String.concat("/", ["", "login"]),
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=
          Fetch.HeadersInit.makeWithDict(
            Js.Dict.fromList(
              Belt.List.keepMap(
                [Some(("Content-Type", "application/json"))], x =>
                x
              ),
            ),
          ),
        ~body=Fetch.BodyInit.make(Js.Json.stringify(encodeLogin(body))),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
    |> then_(response =>
         response |> decodeReturnToken |> (x => Belt_Result.Ok(x)) |> resolve
       )
  );
};

let postRegister = body => {
  Js.Promise.(
    Fetch.fetchWithInit(
      String.concat("/", ["", "register"]),
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=
          Fetch.HeadersInit.makeWithDict(
            Js.Dict.fromList(
              Belt.List.keepMap(
                [Some(("Content-Type", "application/json"))], x =>
                x
              ),
            ),
          ),
        ~body=Fetch.BodyInit.make(Js.Json.stringify(encodeRegister(body))),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
    |> then_(response =>
         response |> decodeReturnToken |> (x => Belt_Result.Ok(x)) |> resolve
       )
  );
};