node {
    checkout scm
    def customImage = docker.build("build-sudoku-scala:${env.BUILD_ID}", "-f .jenkins/docker/Dockerfile .jenkins/docker")
    withCredentials([string(credentialsId: 'coverage-token', variable: 'COVERAGE_TOKEN')]) {
        customImage.inside('-v $HOME/.sbt:/home/jenkins/.sbt') {
            stage('Build') {
               sh 'sbt ++2.11.12 compile'
            }
            stage('Test') {
               sh 'sbt ++2.11.12 test coverage'
            }
            stage('Upload coverage to codecov') {
               sh 'sbt ++2.11.12 coverageReport'
               sh '~/.local/bin/codecov --token $COVERAGE_TOKEN --no-color'
            }
        }
    }
}
