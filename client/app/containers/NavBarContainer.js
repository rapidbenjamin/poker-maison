import { withRouter } from 'react-router-dom'
import { connect } from 'react-redux'

import { logoutUser } from '../actions/auth'
import { isAuthenticated, getUsername } from '../selectors/auth'
import { getPathname } from '../selectors/route'

import NavBar from '../components/NavBar'

const mapStateToProps = state => ({
  isAuthenticated: isAuthenticated(state),
  username: getUsername(state),
  currRoute: getPathname(state)
})

const mapDispatchToProps = (dispatch, { history }) => ({
  logoutUser: () => dispatch(logoutUser(history))
})

export default withRouter(
  connect(
    mapStateToProps,
    mapDispatchToProps
  )(NavBar)
)
