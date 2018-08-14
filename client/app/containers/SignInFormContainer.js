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
    this.props.login(
      username,
      password
    )
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
  login: (username, password) => dispatch(login(username, password, history))
})

export default connect(
  undefined,
  mapDispatchToProps
)(withRouter(SignInFormContainer))
