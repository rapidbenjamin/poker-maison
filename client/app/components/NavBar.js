import React from 'react'

const NavBar = ({ authenticated, username, history }) => (
  <nav role="navigation" className="navbar">
    <div className="navbar-brand">
      <a
        role="button"
        className="navbar-item" onClick={() => history.push('/')}>
        <img
          src="https://bulma.io/images/bulma-logo.png"
          alt="Bulma: a modern CSS framework based on Flexbox"
          width="112"
          height="28"
        />
      </a>
    </div>
    <div className="navbar-menu">
      <div className="navbar-start">
        <a
          role="button"
          className="navbar-item" onClick={() => history.push('lobby')}>
          Lobby
        </a>
        <a
          role="button"
          className="navbar-item" onClick={() => history.push('games')}>
          Games
        </a>
      </div>
      <div className="navbar-end">
        {authenticated ? (
          <a
            role="button"
            className="navbar-item" onClick={() => history.push('profile')}>
            {`Logged in as ${username}`}
          </a>
        ) : (
          ''
        )}
        {!authenticated ? (
          <a
            role="button"
            className="navbar-item" onClick={() => history.push('signin')}>
            Login
          </a>
        ) : (
          ''
        )}
        {!authenticated ? (
          <a
            role="button"
            className="navbar-item" onClick={() => history.push('signup')}>
            Register
          </a>
        ) : (
          ''
        )}
      </div>
    </div>
  </nav>
)

export default NavBar
