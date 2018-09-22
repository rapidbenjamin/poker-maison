import React from 'react'
import { connect } from 'react-redux'

import { getUsername } from '../selectors/auth'
import { getProfileSelector } from '../selectors/profile'

import { getProfile } from '../actions/profile'
import Profile from '../components/Profile'

class ProfileContainer extends React.Component {
  componentDidMount() {
    this.props.getProfile()
  }

  render() {
    return <Profile {...this.props} />
  }
}

const mapStateToProps = state => ({
  username: getUsername(state),
  profile: getProfileSelector(state)
})

const mapDispatchToProps = (dispatch, { username }) => ({
  getProfile: () => dispatch(getProfile(username))
})

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(ProfileContainer)
