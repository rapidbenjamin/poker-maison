
import React, { Component } from "react";
import { connect } from "react-redux";
import { withRouter } from 'react-router'

import { isAuthenticated } from "../selectors/auth";
import { isSocketAuthenticated } from "../selectors/socket";

import { connectSocket } from "../actions/auth";

import App from '../components/App'

class AppContainer extends Component {
  //componentDidMount() {
  //  /* If we have a token and socket not connected then try and connect */
  //  const { connectSocket, isSocketAuthenticated } = this.props
  //  const token = localStorage.getItem("token");
  //  if (token && !isSocketAuthenticated) {
  //    connectSocket(token.access_token);
  //  }
  //}

  render() {
    return <App />;
  }
}

const mapStateToProps = state => ({
  isAuthenticated: isAuthenticated(state),
  isSocketAuthenticated: isSocketAuthenticated(state)
});

const mapDispatchToProps = dispatch => ({
  connectSocket: token => dispatch(connectSocket(token))
});

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(AppContainer));
