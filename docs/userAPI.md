
## POST /register

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Sample Register (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{
      newUserUsername: "Argo",
      newUserEmail: "gooby@goo.com",
      newUserpassword: "password123"
}
    ```

### Response:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Sample ReturnToken (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{
    "access_token": "eyJhbGciOiJIUzI1NiIs",
    "expiration": 3600,
    "refresh_token": "EwMIjImdgoeswazNQx"
}

    ```


## POST /login

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Sample Login (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{
      loginEmail: "gooby@goo.com",
      loginPassword: "password123"
}
    ```

### Response:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Sample ReturnToken (`application/json;charset=utf-8`, `application/json`):

    ```javascript
{
    "access_token": "eyJhbGciOiJIUzI1NiIs",
    "expiration": 3600,
    "refresh_token": "EwMIjImdgoeswazNQx"
}

    ```


