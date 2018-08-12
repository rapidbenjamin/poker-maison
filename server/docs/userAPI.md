## POST /register

### Request:

- Supported content types are:

  - `application/json;charset=utf-8`
  - `application/json`

- Sample Register (`application/json;charset=utf-8`, `application/json`):

      ```javascript

  {
  newUsername: "Argo",
  newEmail: "gooby@goo.com",
  newPassword: "password123"
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

## GET /profile

### Request:

- Supported content types are:

  - `application/json;charset=utf-8`
  - `application/json`

- Headers:
  - Authorization: eyJhbGciOiJIUzI1NiIs

Ensure access token is in Authorization header

### Response:

- Supported content types are:

  - `application/json;charset=utf-8`
  - `application/json`

- Sample ReturnToken (`application/json;charset=utf-8`, `application/json`):

      ```javascript

  {
  "proChips": 3000,
  "proUsername": "Argo",
  "proEmail": "gooby@goo.com"
  }

      ```
