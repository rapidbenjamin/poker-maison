import React from 'react';

import { Footer as BulmaFooter, Container, Content } from 'react-bulma-components/full';

const Footer = () => (
  <BulmaFooter>
    <Container>
      <Content style={{ textAlign: "center" }}>
        <p>
          <strong>
            Poker App - therewillbecode
          </strong>
        </p>
      </Content>
    </Container>
  </BulmaFooter>
);

export default Footer;
