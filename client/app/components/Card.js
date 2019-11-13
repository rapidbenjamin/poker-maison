import React from 'react';

const showRank = rank => {
  switch (rank) {
    case 'Ace':
      return 'A'
    case 'King':
      return 'K'
    case 'Queen':
      return 'Q'
    case 'Jack':
      return 'J'
    case 'Ten':
      return '10'
    case 'Nine':
      return '9'
    case 'Eight':
      return '8'
    case 'Seven':
      return '7'
    case 'Six':
      return '6'
    case 'Five':
      return '5'
    case 'Four':
      return '4'
    case 'Three':
      return '3'
    case 'Two':
      return '2'
  }
}

const Card = ({ rank, suit }) => (
  <div className='card'>
    <div className="rank">
      <span className="monospace-font-bold">{showRank(rank)}</span>
    </div>
    <div className="suit">
      <img alt={suit} src={`http://localhost:9080/${suit}.svg`} />
    </div>
  </div>);

export default Card;
