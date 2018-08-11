import React from 'react';
import { connect } from "react-redux";
import { withRouter } from 'react-router';

import { login } from '../actions/auth'
import SignInForm from '../components/SignInForm'

class SignInFormContainer extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      email: "",
      password: ""
    };
  }

  handleChange = event =>
    this.setState({
      [event.target.name]: event.target.value
    });

  handleSubmit = event => {
    alert('A name was submitted: ' + this.state.value);
    event.preventDefault();
  }

  validateForm = () => {
    return this.state.email.length > 0 && this.state.password.length > 0;
  }

  render() {
    return <SignInForm handleChange={this.handleChange} handleSubmit={this.handleSubmit} />
  }
}

const mapDispatchToProps = (dispatch, { history }) => ({ login: credentials => dispatch(login({ ...credentials }, history)) })

export default connect(undefined, mapDispatchToProps)(withRouter(SignInFormContainer));