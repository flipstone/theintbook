# theintbook

The Integral Social Network

## Development - TL;DR

### Main shell
    docker-compose up -d

### Frontend Shell
    docker attach theintbook_frontend_1
    cabal install
    
### Backend Shell
    docker attach theintbook_backend_1
    cabal install
    cabal repl
    :main
    
### Browser

    http://localhost:8000
    
