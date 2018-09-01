import React from 'react'

// TODO move to own component called pocket cards
import Card from './Card'

const getPocketCards = cards =>
  cards !== undefined && cards !== null ? cards.map(card => {
    console.log(card.toJS())
    const rank = card.get('rank')
    const suit = card.get('suit')

    return (<Card
      key={rank + suit}
      rank={rank}
      suit={suit}
    />)
  }) : ''


const ActionPanel = ({
  handleChange,
  betValue,
  bet,
  raise,
  call,
  fold,
  check,
  postSmallBlind,
  postBigBlind,
  sitDown,
  leaveGameSeat,
  userPocketCards
}) => {
  console.log(userPocketCards)

  return (<div className='actionPanel'>
    <div className='action-panel-left-container'>
      {userPocketCards ? userPocketCards.map(card => {
        console.log(card.toJS())
        const rank = card.get('rank')
        const suit = card.get('suit')

        return (<Card
          key={rank + suit}
          rank={rank}
          suit={suit}
        />)
      }) : ''}
    </div>
    <div className='user-actions-container'>
      <div>
        <button
          type="button"
          onClick={() => postBigBlind()} className="button">
          postBigBlind
    </button>
        <button
          type="button" onClick={() => postSmallBlind()} className="button">
          postSmallBlind
    </button>
        <button type="button" onClick={() => check()} className="button">
          check
    </button>
        <button type="button" onClick={() => call()} className="button">
          call</button>
        <button
          type="button"
          onClick={() => bet(betValue)} className="button">Bet {betValue}</button>
        <button
          type="button"
          onClick={() => raise(betValue)}
          className="button">
          Raise {betValue}</button>
        <button
          type="button"
          onClick={() => fold()}
          className="button">
          Fold
    </button>
        <input
          type="text"
          value={betValue}
          onChange={handleChange}
        />
        <button
          type="button"
          onClick={() => sitDown(betValue)}
          className="button">
          SitDown {betValue}
        </button>
        <button
          type="button"
          onClick={() => leaveGameSeat()}
          className="button">
          LeaveGame
    </button>
      </div>
    </div>
  </div>)
}


export default ActionPanel
