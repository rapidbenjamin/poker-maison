import React from 'react';
import { Navbar } from 'react-bulma-components/full';

const NavBar = () => (
  <Navbar>
    <Navbar.Brand>
      <Navbar.Item href="#">
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
        <Navbar.Item href="#">
          Lobby
        </Navbar.Item>
        <Navbar.Item href="#">
          Games
        </Navbar.Item>
      </Navbar.Container>
      <Navbar.Container position="end">
        <Navbar.Item href="#">
          this is aligned to the right
        </Navbar.Item>
      </Navbar.Container>
    </Navbar.Menu>
  </Navbar>
);

export default NavBar;
