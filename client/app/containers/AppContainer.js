import React, { Component } from 'react'
import { connect } from 'react-redux'
import { withRouter } from 'react-router'

import { isAuthenticated, getUsername } from '../selectors/auth'
import { isSocketAuthenticated } from '../selectors/socket'

import { connectSocket } from '../actions/auth'

import App from '../components/App'

class AppContainer extends Component {
  componentDidMount() {
    /* If we have a token and socket not connected then try and connect */
    const { connectSocket, isSocketAuthenticated } = this.props
    const token = localStorage.getItem('token')
    if (token && !isSocketAuthenticated) {
      try {
        const { access_token } = JSON.parse(token)
        connectSocket(access_token)
      } catch (e) {
        console.log(e)
      }
    }
  }

  render() {
    const { username } = this.props

    return <App username={username} />
  }
}

const mapStateToProps = state => ({
  isAuthenticated: isAuthenticated(state),
  isSocketAuthenticated: isSocketAuthenticated(state),
  username: getUsername(state)
})

const mapDispatchToProps = dispatch => ({
  connectSocket: (url, token) => dispatch(connectSocket(url, token))
})

export default withRouter(
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(AppContainer)
)
