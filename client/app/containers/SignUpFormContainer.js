import React from 'react'
import { connect } from 'react-redux'
import { withRouter } from 'react-router'

import { register } from '../actions/auth'
import SignUpForm from '../components/SignUpForm'

class SignUpFormContainer extends React.Component {
  constructor(props) {
    super(props)

    this.state = {
      email: '',
      username: '',
      password: '',
      repeatPassword: ''
    }
  }

  handleChange = event =>
    this.setState({
      [event.target.name]: event.target.value
    })

  handleSubmit = event => {
    const { username, email, password } = this.state
    this.props.register({
      body: {
        newUsername: username,
        newUserEmail: email,
        newUserPassword: password
      }
    })
    event.preventDefault()
  }

  validateForm = () => {
    return this.state.email.length > 0 && this.state.password.length > 0
  }

  render() {
    return (
      <SignUpForm
        handleChange={this.handleChange}
        handleSubmit={this.handleSubmit}
      />
    )
  }
}

const mapDispatchToProps = (dispatch, { history }) => ({
  register: credentials => dispatch(register({ ...credentials }, history))
})

export default connect(
  undefined,
  mapDispatchToProps
)(withRouter(SignUpFormContainer))
