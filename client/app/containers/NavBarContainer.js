import { withRouter } from 'react-router-dom'
import { connect } from 'react-redux'

import NavBar from '../components/NavBar'

const mapStateToProps = state => ({
    authenticated: state.get('authenticated'),
    username: state.get('username'),
})

export default connect(mapStateToProps)(withRouter(NavBar));