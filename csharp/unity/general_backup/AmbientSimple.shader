// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Custom/AmbientSimple" {
	Properties {
		_AmbientLightColor ("Ambient Light Color", Color) = (1,1,1,1)
		_AmbientLightIntensity ("Ambient Light Intensity", Range(0.0,1.0)) = 0.5
	}
	SubShader {
		Pass {
			CGPROGRAM
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag

			fixed4 _AmbientLightColor;
			float _AmbientLightIntensity;

			float4 vert(float4 v:POSITION) : SV_POSITION {
				return UnityObjectToClipPos(v);
			}

			fixed4 frag() : SV_TARGET {
				return _AmbientLightColor * _AmbientLightIntensity;
			}
			ENDCG
		}
	}

}
