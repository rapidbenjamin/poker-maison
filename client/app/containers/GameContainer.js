import React from 'react'
import { connect } from "react-redux";

import { getGame } from '../selectors/games'

class GameContainer extends React.Component {

    render() {
        const { match: { params: { tableName } } } = this.props

        console.log(tableName)
        return (<div>ggggg</div>)
    }
}

const mapStateToProps = (state, { match: { params: { tableName } } }) => ({
    game: getGame(state, tableName)
});

const mapDispatchToProps = (dispatch, { match: { params: { tableName } } }) => ({
    // game: () => dispatch(getLobby()),
});


export default connect(mapStateToProps, mapDispatchToProps)(GameContainer)
