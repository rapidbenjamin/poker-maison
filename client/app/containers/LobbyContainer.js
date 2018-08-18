import React from 'react'
import { connect } from "react-redux";
import { withRouter } from 'react-router-dom'

import { getLobby, subscribeToTable } from '../actions/lobby'
import { getLobbyState } from '../selectors/lobby'
import Lobby from '../components/Lobby'

class LobbyContainer extends React.Component {
  componentDidMount() {
    this.props.getLobby()
  }

  render() {
    const { lobby } = this.props
    return (<Lobby {...this.props} />)
  }
}

const mapStateToProps = state => ({
  lobby: getLobbyState(state)
});

const mapDispatchToProps = (dispatch) => ({
  getLobby: () => dispatch(getLobby()),
  subscribeToATable: tableName => dispatch(subscribeToTable(tableName))
});

export default withRouter(connect(mapStateToProps, mapDispatchToProps)(LobbyContainer));
