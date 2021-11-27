node {
    checkout scm
    def customImage = docker.build("build-sudoku-scala:${env.BUILD_ID}", "-f .jenkins/docker/Dockerfile .jenkins/docker")
    withCredentials([string(credentialsId: 'coverage-token', variable: 'COVERAGE_TOKEN')]) {
        customImage.inside('-v $HOME/.sbt:/home/jenkins/.sbt') {
            stage('Build') {
               sh 'sbt -no-colors ++2.12.14 compile'
            }
            stage('Test') {
               sh 'sbt -no-colors ++2.12.14 test coverage'
            }
            stage('Upload coverage to codecov') {
               sh 'sbt -no-colors ++2.12.14 coverageReport'
               sh '~/.local/bin/codecov --token $COVERAGE_TOKEN --no-color'
            }
        }
    }
}
