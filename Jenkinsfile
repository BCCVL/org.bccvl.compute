

if (env.BRANCH_NAME == 'master') {

    node('docker') {
        stage('Checkout') {
            // clean git clone, but don't fail in case it doesn't exist yet
            sh(script: 'git clean -x -d -f', returnStatus: true)
            checkout scm
        }
        // TODO: we should do some package verification here?
        def img = docker.image('python:2')
        img.inside() {
            stage('Package') {
                if (publishPackage(currentBuild.result, env.BRANCH_NAME)) {
                    withPyPi() {
                        sh 'rm -fr build dist'
                        sh 'python setup.py register -r devpi sdist bdist_wheel upload -r devpi'
                    }
                }
            }
        }
    }

} else {

  echo sh(returnStdout: true, script: 'env')

    def downstream = build(
        //job: "../bccvl_buildout/${java.net.URLEncoder.encode(env.BRANCH_NAME)}",
        job: "../bccvl_buildout/${java.net.URLEncoder.encode('feature/develop_docker')}",
        wait: true,
        propagate: true
    )


}

