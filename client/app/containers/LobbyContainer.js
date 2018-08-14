import React from 'react'
import { connect } from "react-redux";

import { getLobby } from '../actions/lobby'
import { getLobbyState } from '../selectors/lobby'
import Lobby from '../components/Lobby'

class LobbyContainer extends React.Component {
  componentDidMount() {
    this.props.getLobby()
  }

  render() {
    return (<Lobby />)
  }
}

const mapStateToProps = state => ({
  lobby: () => getLobbyState(state)
});

const mapDispatchToProps = dispatch => ({
  getLobby: () => dispatch(getLobby())
});

export default connect(mapStateToProps, mapDispatchToProps)(LobbyContainer);
