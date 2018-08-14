
import React, { Component } from "react";
import { connect } from "react-redux";
import { withRouter } from 'react-router'

import { isAuthenticated } from "../selectors/auth";
import { isSocketAuthenticated } from "../selectors/socket";

import { connectSocket } from "../actions/auth";

import App from '../components/App'

/* Action Creators for Socket API authentication */
const SOCKET_API_URL = process.env.SOCKET_API_URL || 'http://localhost:5000'

class AppContainer extends Component {
  componentDidMount() {
    /* If we have a token and socket not connected then try and connect */
    const { connectSocket, isSocketAuthenticated } = this.props
    const token = localStorage.getItem("token");
    if (token && !isSocketAuthenticated) {
      const { access_token } = JSON.parse(token)
      connectSocket(SOCKET_API_URL, access_token);
    }
  }

  render() {
    return <App />;
  }
}

const mapStateToProps = state => ({
  isAuthenticated: isAuthenticated(state),
  isSocketAuthenticated: isSocketAuthenticated(state)
});

const mapDispatchToProps = dispatch => ({
  connectSocket: (url, token) => dispatch(connectSocket(url, token))
});

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(AppContainer));
