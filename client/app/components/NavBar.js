import React from 'react'

const NavBar = ({
  isAuthenticated,
  currRoute,
  username,
  history,
  logoutUser
}) => (
    <nav role="navigation" className="navbar">
      <div className="navbar-brand">
        <div>
          <a onClick={() => history.push('/')}>
            <h4 className="brand-title">10 Poker</h4>
          </a>
        </div>
      </div>
      <div className="navbar-menu">
        <div className="navbar-start">
          {isAuthenticated ? (
            <React.Fragment>
              <div
                onClick={() => history.push('/lobby')}
                className={`navbar-item${
                  currRoute === '/lobby' ? '-active' : ''
                  }`}
              >
                <a >
                  <h4>Lobby</h4>
                </a>
              </div>

            </React.Fragment>
          ) : (
              ''
            )}
        </div>
        <div className="navbar-end">
          {isAuthenticated ? (
            <div
              className={`navbar-item${
                currRoute === '/profile' ? '-active' : ''
                }`}
            >
              {' '}
              <a onClick={() => history.push('/profile')}>
                <h4>
                  <span className="navbar-username">{username}</span>
                </h4>
              </a>
            </div>
          ) : (
              ''
            )}
          {isAuthenticated ? (
            <div className="navbar-item">
              <a onClick={() => logoutUser()}>
                <h4>Logout</h4>
              </a>
            </div>
          ) : (
              ''
            )}
          {!isAuthenticated ? (
            <div className="navbar-item">
              <a onClick={() => history.push('/signin')}>
                <h4>Login</h4>
              </a>
            </div>
          ) : (
              ''
            )}
          {!isAuthenticated ? (
            <div className="navbar-item">
              <a onClick={() => history.push('/signup')}>
                <h4>Register</h4>
              </a>
            </div>
          ) : (
              ''
            )}
        </div>
      </div>
    </nav>
  )

export default NavBar
