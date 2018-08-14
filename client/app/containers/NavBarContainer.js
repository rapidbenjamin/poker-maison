import { withRouter } from 'react-router-dom'
import { connect } from 'react-redux'

import { isAuthenticated, getUsername } from '../selectors/auth'

import NavBar from '../components/NavBar'

const mapStateToProps = state => ({
  isAuthenticated: isAuthenticated(state),
  username: getUsername(state),
})

export default connect(mapStateToProps)(withRouter(NavBar));