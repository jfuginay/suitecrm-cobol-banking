FROM ubuntu:22.04

# Install GnuCOBOL and Node.js
RUN apt-get update && apt-get install -y \
    gnucobol \
    nodejs \
    npm \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create app directory
WORKDIR /app

# Copy COBOL services
COPY ./cobol-services /app

# Install Node dependencies if package.json exists
RUN if [ -f package.json ]; then npm install; fi

# Compile COBOL programs
RUN for cob in *.cob; do \
        if [ -f "$cob" ]; then \
            cobc -x -o "${cob%.cob}" "$cob"; \
        fi \
    done

EXPOSE 3000

# Start the API server
CMD ["npm", "start"]