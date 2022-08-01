node {
    checkout scm
    def userId = sh(script: "id -u ${USER}", returnStdout: true).trim()
    def customImage = docker.build("build-sudoku-scala:${env.BUILD_ID}", "--build-arg JENKINS_UID=${userId} -f .jenkins/docker/Dockerfile .jenkins/docker")

    sh 'mkdir -p ${WORKSPACE_TMP}/sbt'

    withCredentials([string(credentialsId: 'coverage-token', variable: 'COVERAGE_TOKEN')]) {
        cache(maxCacheSize: 250, defaultBranch: 'master', caches: [
            [$class: 'ArbitraryFileCache', path: "${env.WORKSPACE_TMP}/sbt", cacheValidityDecidingFile: 'project/plugins.sbt', compressionMethod: 'TARGZ']
        ]) {
            customImage.inside("-v ${env.WORKSPACE_TMP}/sbt:/home/jenkins/.sbt") {
                stage('Build') {
                   sh 'sbt -no-colors ++3.1.3 compile'
                }
                stage('Test') {
                   sh 'sbt -no-colors ++3.1.3 test coverage'
                }
                stage('Upload coverage to codecov') {
                   sh 'sbt -no-colors ++3.1.3 coverageReport'
                   sh '~/.local/bin/codecov --token $COVERAGE_TOKEN --no-color'
                }
            }
        }
    }
}
