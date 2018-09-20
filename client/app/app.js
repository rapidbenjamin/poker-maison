/**
 * app.js
 *
 * This is the entry file for the application, only setup and boilerplate
 * code.
 */

// Needed for redux-saga es6 generator support
import 'babel-polyfill'

// Import all the third party stuff
import React from 'react'
import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import { ConnectedRouter } from 'react-router-redux'
import createHistory from 'history/createBrowserHistory'

import 'sanitize.css/sanitize.css'

import AppContainer from 'containers/AppContainer'

import { authSuccess } from './actions/auth'

import 'styles/main.scss'

import configureStore from './configureStore'

// Create redux store with history
const initialState = {}
const history = createHistory()
const store = configureStore(initialState, history)
const MOUNT_NODE = document.getElementById('app')

const render = () => {
  ReactDOM.render(
    <Provider store={store}>
      <ConnectedRouter history={history}>
        <AppContainer />
      </ConnectedRouter>
    </Provider>,
    MOUNT_NODE
  )
}

if (module.hot) {
  // Hot reloadable React components and translation json files
  // modules.hot.accept does not accept dynamic dependencies,
  // have to be constants at compile-time
  module.hot.accept(['containers/AppContainer'], () => {
    ReactDOM.unmountComponentAtNode(MOUNT_NODE)
    render()
  })
}

// If we have a JWT token in localStorage then treat user as authenticated
const token = localStorage.getItem('token')

if (token) {
  try {
    const { username } = JSON.parse(localStorage.getItem('token'))
    store.dispatch(authSuccess(username))
  } catch (e) {
    console.log(e)
  }
}

render()
