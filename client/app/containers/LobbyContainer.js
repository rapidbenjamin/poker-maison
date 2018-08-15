import React from 'react'
import { connect } from "react-redux";

import { getLobby } from '../actions/lobby'
import { getLobbyTables } from '../selectors/lobby'
import Lobby from '../components/Lobby'

class LobbyContainer extends React.Component {
  componentDidMount() {
    this.props.getLobby()
  }

  render() {
    const { lobby } = this.props
    return (<Lobby lobby={lobby} />)
  }
}

const mapStateToProps = state => ({
  lobby: getLobbyTables(state)
});

const mapDispatchToProps = dispatch => ({
  getLobby: () => dispatch(getLobby())
});

export default connect(mapStateToProps, mapDispatchToProps)(LobbyContainer);
