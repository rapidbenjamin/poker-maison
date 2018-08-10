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
      <Navbar.Burger />
    </Navbar.Brand>
    <Navbar.Menu>
      <Navbar.Container>
        <Navbar.Item dropdown hoverable>
          <Navbar.Link>
            Docs
          </Navbar.Link>
          <Navbar.Dropdown boxed>
            <Navbar.Item href="#">
              Home
            </Navbar.Item>
            <Navbar.Item href="#">
              List
            </Navbar.Item>
            <Navbar.Item href="#">
              Another Item
            </Navbar.Item>
            <Navbar.Divider />
            <Navbar.Item active href="#">
              Active
            </Navbar.Item>
          </Navbar.Dropdown>
        </Navbar.Item>
        <Navbar.Item href="#">
          Second
        </Navbar.Item>
      </Navbar.Container>
      <Navbar.Container position="end">
        <Navbar.Item dropdown hoverable>
          <Navbar.Link>
            Other Menu
          </Navbar.Link>
          <Navbar.Dropdown right boxed>
            <Navbar.Item href="#">
              this is aligned to the right
            </Navbar.Item>
          </Navbar.Dropdown>
        </Navbar.Item>
      </Navbar.Container>
    </Navbar.Menu>
  </Navbar>
);

export default NavBar;
