import { withRouter } from 'react-router-dom'
import { connect } from 'react-redux'

import NavBar from '../components/NavBar'

const mapStateToProps = ({ auth: authenticated }) => ({ authenticated })

export default connect(mapStateToProps)(withRouter(NavBar));