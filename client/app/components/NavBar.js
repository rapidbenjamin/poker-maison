import React from 'react';
import { Navbar } from 'react-bulma-components/full';

const NavBar = ({ authenticated, username, history }) => (
  <Navbar>
    <Navbar.Brand>
      <Navbar.Item onClick={() => history.push('/')}>
        <img
          src="https://bulma.io/images/bulma-logo.png"
          alt="Bulma: a modern CSS framework based on Flexbox"
          width="112"
          height="28"
        />
      </Navbar.Item>
    </Navbar.Brand>
    <Navbar.Menu>
      <Navbar.Container>
        <Navbar.Item onClick={() => history.push('lobby')}>
          Lobby
        </Navbar.Item>
        <Navbar.Item onClick={() => history.push('games')}>
          Games
        </Navbar.Item>
      </Navbar.Container>
      <Navbar.Container position="end">
        {authenticated ?
          <Navbar.Item onClick={() => history.push('profile')}>
            {`Logged in as ${username}`}
          </Navbar.Item>
          : ''}
        {!authenticated ?
          <Navbar.Item onClick={() => history.push('signin')}>
            Login
          </Navbar.Item> : ''}
        {!authenticated ?
          <Navbar.Item onClick={() => history.push('signup')}>
            Register
          </Navbar.Item> : ''
        }
      </Navbar.Container>
    </Navbar.Menu>
  </Navbar>
);

export default NavBar;
