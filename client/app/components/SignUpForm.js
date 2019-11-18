import React from 'react'

const SignUpForm = ({ handleChange, handleSubmit }) => (
  <div className="form-container">
    <div className="form">
      <h2> Sign Up</h2>

      <form onSubmit={handleSubmit}>
        <div className="field">
          <p className="control has-icons-left has-icons-right">
            <input
              className="input"
              type="email"
              placeholder="Email"
              name="email"
              onChange={e => handleChange(e)}
            />
            <span className="icon is-small is-left">
              <i className="fas fa-envelope" />
            </span>
            <span className="icon is-small is-right">
              <i className="fas fa-check" />
            </span>
          </p>
        </div>
        <div className="field">
          <p className="control has-icons-left">
            <input
              className="input"
              type="text"
              placeholder="Username"
              name="username"
              onChange={e => handleChange(e)}
            />
            <span className="icon is-small is-left">
              <i className="fas fa-lock" />
            </span>
          </p>
        </div>
        <div className="field">
          <p className="control has-icons-left">
            <input
              className="input"
              type="password"
              placeholder="Password"
              name="password"
              onChange={e => handleChange(e)}
            />
            <span className="icon is-small is-left">
              <i className="fas fa-lock" />
            </span>
          </p>
        </div>
        <div className="field">
          <p className="control has-icons-left">
            <input
              className="input"
              type="password"
              placeholder="Repeat Password"
              name="repeatPassword"
              onChange={e => handleChange(e)}
            />
            <span className="icon is-small is-left">
              <i className="fas fa-lock" />
            </span>
          </p>
        </div>
        <div className="field">
          <p className="control">
            <button className="button is-success">Register</button>
          </p>
        </div>
      </form>
    </div>
  </div>
)

export default SignUpForm
