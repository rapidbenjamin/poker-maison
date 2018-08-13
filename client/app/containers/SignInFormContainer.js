import React from 'react'
import { connect } from 'react-redux'
import { withRouter } from 'react-router'

import { login } from '../actions/auth'
import SignInForm from '../components/SignInForm'

class SignInFormContainer extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      username: '',
      password: ''
    }
  }

  handleChange = event =>
    this.setState({
      [event.target.name]: event.target.value
    })

  handleSubmit = event => {
    const { username, password } = this.state
    this.props.login({
      loginUsername: username,
      loginPassword: password
    })
    event.preventDefault()
  }

  validateForm = () => {
    return this.state.username.length > 0 && this.state.password.length > 0
  }

  render() {
    return (
      <SignInForm
        handleChange={this.handleChange}
        handleSubmit={this.handleSubmit}
      />
    )
  }
}

const mapDispatchToProps = (dispatch, { history }) => ({
  login: credentials => dispatch(login({ ...credentials }, history))
})

export default connect(
  undefined,
  mapDispatchToProps
)(withRouter(SignInFormContainer))
