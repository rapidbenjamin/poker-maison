/**
 *
 * App
 *
 * This component is the skeleton around the actual pages, and should only
 * contain code that should be seen on all pages. (e.g. navigation bar)
 */

import React from 'react'
import { Helmet } from 'react-helmet'
import { Switch, Route } from 'react-router-dom'

import HomeContainer from '../containers/HomeContainer'
import NavBarContainer from '../containers/NavBarContainer'
import SignUpFormContainer from '../containers/SignUpFormContainer'
import SignInFormContainer from '../containers/SignInFormContainer'
import LobbyContainer from '../containers/LobbyContainer'
import GameContainer from '../containers/GameContainer'
import ProfileContainer from '../containers/ProfileContainer'

import Footer from './Footer'
import NotFoundPage from './NotFoundPage'
import Signout from './Signout'

const App = ({ username }) => (
  <div className="app-wrapper">
    <Helmet
      titleTemplate="%s - React.js Boilerplate"
      defaultTitle="React.js Boilerplate"
    >
      <meta name="description" content="A React.js Boilerplate application" />
    </Helmet>
    <NavBarContainer />
    <Switch>
      <Route exact path="/" component={HomeContainer} />
      <Route path="/signup" component={SignUpFormContainer} />
      <Route path="/signin" component={SignInFormContainer} />
      <Route path="/signout" component={Signout} />
      <Route path="/lobby" component={LobbyContainer} />
      <Route path="/profile" component={ProfileContainer} />
      <Route
        path="/game/:tableName"
        render={props => <GameContainer {...props} username={username} />}
      />
      <Route path="" component={NotFoundPage} />
    </Switch>
  </div>
)

export default App
