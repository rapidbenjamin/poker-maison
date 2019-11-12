import React from 'react';

let isPlayerInactive = playerState =>
  playerState === 'Folded' ||
  playerState === 'SatOut'


const Seat = ({ playerName, chips, isTurnToAct, hasPocketCards, position, playerState }) => (
  <div className={`seat-${position}-container`}>
    {hasPocketCards ?
      <div className='hidden-pocket-cards' >
        <div className='hidden-pocket-cards-container' >
          <div className='card pocket-one' />
          <div className='card pocket-two' />
        </div>
      </div> : ''}
    <div
      className={`seat-${position}
        ${isTurnToAct ? 'active-player' : ''} 
        ${playerName ? '' : 'empty-seat'}
        ${isPlayerInactive(playerState) ? 'disabled' : ''}`
      }>
      <h5 className={playerName ? 'player-name' : ''}>{playerName || 'Take Seat'}</h5>
      <h5 className={playerState ? 'player-state' : ''}>
        {playerState == 'In' && chips == 0 ? 'All In' : ''}
        {playerState == 'Folded' ? 'Folded' : ''}
        {playerState == 'SatOut' ? 'Sat Out' : ''}</h5>
      {playerName ? <h5 className='player-chip-count'>
        <span className='monospaced-font'>
          {playerState == 'In' && chips == 0 ? '' : chips}</span></h5> : ''}
    </div>
  </div>);

export default Seat;