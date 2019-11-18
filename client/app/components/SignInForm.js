import React from 'react';

const SignInForm = ({ handleChange, handleSubmit }) => (
  <div className="form-container">
    <div className="form" >
      <div className="form-container">

        <h2> Log In</h2>

        <form onSubmit={handleSubmit}>
          <div className="field">
            <p className="control has-icons-left has-icons-right">
              <input className="input" type="text" placeholder="Username" name="username" onChange={e => handleChange(e)} />
              <span className="icon is-small is-left">
                <i className="fas fa-envelope"></i>
              </span>
              <span className="icon is-small is-right">
                <i className="fas fa-check"></i>
              </span>
            </p>
          </div>
          <div className="field">
            <p className="control has-icons-left">
              <input className="input" type="password" placeholder="Password" name="password" onChange={e => handleChange(e)} />
              <span className="icon is-small is-left">
                <i className="fas fa-lock"></i>
              </span>
            </p>
          </div>
          <div className="field">
            <p className="control">
              <button className="button is-success">
                Login
        </button>
            </p>
          </div>
        </form>
      </div>
    </div>
  </div>
);

export default SignInForm;
