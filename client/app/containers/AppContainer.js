
import React, { Component } from "react";
import { connect } from "react-redux";
import { withRouter } from 'react-router'

import { isAuthenticated } from "../selectors/auth";
import { connectSocket } from "../actions/auth";

import App from '../components/App'

class AppContainer extends Component {
  componentDidMount() {
    const token = localStorage.getItem("token");
    // If we have a token, consider the user to be signed in and update state
    if (token && !this.props.isAuthenticated) {
      this.props.connectSocket();
    }
  }

  render() {
    return <App />;
  }
}

const mapStateToProps = state => ({ isAuthenticated: isAuthenticated(state) });

const mapDispatchToProps = dispatch => ({
  connectSocket: token => dispatch(connectSocket(token))
});

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(AppContainer));
