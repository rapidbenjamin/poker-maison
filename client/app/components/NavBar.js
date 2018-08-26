import React from 'react'

const NavBar = ({ isAuthenticated, username, history, logoutUser }) => (
  <nav role="navigation" className="navbar">
    <div className="navbar-brand">
      <div>
        <a onClick={() => history.push('/')}>
          <h2>PokerSpace</h2>
        </a>
      </div>
    </div>
    <div className="navbar-menu">
      <div className="navbar-start">
        <div>
          <div className="navbar-item">
            <a onClick={() => history.push('lobby')} >
              <h3>
                Lobby
              </h3>
            </a>
          </div>
        </div>
        <div className="navbar-item">
          <a onClick={() => history.push('game')}>
            <h3>
              Game
          </h3>
          </a>
        </div>
      </div>
      <div className="navbar-end">
        {isAuthenticated ? (
          <div className="navbar-item">
            <a onClick={() => history.push('profile')}>
              <h3>
                {username}
              </h3>
            </a>
          </div>
        ) : (
            ''
          )}
        {isAuthenticated ? (
          <div className="navbar-item">
            <a onClick={() => logoutUser()}>
              <h3>
                Logout
              </h3>
            </a>
          </div>
        ) : (
            ''
          )}
        {!isAuthenticated ? (
          <div className="navbar-item">
            <a onClick={() => history.push('signin')}>
              <h3>
                Login
              </h3>
            </a>
          </div>
        ) : (
            ''
          )}
        {!isAuthenticated ? (
          <div className="navbar-item">
            <a onClick={() => history.push('signup')}>
              <h3>
                Register
              </h3>
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
