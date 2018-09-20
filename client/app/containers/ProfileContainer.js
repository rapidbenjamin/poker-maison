import React from 'react'
import { connect } from 'react-redux'

import { getUsername } from '../selectors/auth'
import { getProfile } from '../selectors/profile'

import Profile from '../components/Profile'

class ProfileContainer extends React.Component {
  componentDidMount() {
    // this.getProfile()
  }

  render() {
    return <Profile {...this.props} />
  }
}

const mapStateToProps = state => ({
  username: getUsername(state),
  profile: getProfile(state)
})

const mapDispatchToProps = (dispatch, { username }) => ({
  getProfile: () => dispatch(getProfile(username))
})

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(ProfileContainer)
