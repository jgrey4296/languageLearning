Shader "Custom/DiffuseSimple" {
	Properties {
		_AmbientLightColor ("Ambient Light Color", Color) = (1,1,1,1)
		_AmbientLightIntensity("Ambient Light Intensity", Range(0,1)) = 1

		_DiffuseDirection("Diffuse Light Direction", Vector) = (0.22, 0.84, 0.78, 1)
		_DiffuseColor("Diffuse Light Color",Color) = (1,1,1,1)
		_DiffuseIntensity("Diffuse Light Intensity", Range(0,1)) = 1
	}
	SubShader {
		Pass {
			CGPROGRAM
			#include "UnityCG.cginc"
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag

			float4 _AmbientLightColor;
			float _AmbientLightIntensity;
			float3 _DiffuseDirection;
			float4 _DiffuseColor;
			float _DiffuseIntensity;

			struct vsIn {
				float4 position : POSITION;
				float3 normal : NORMAL;
			};

			struct vsOut {
				float4 position : SV_POSITION;
				float3 normal : NORMAL;
			};

			vsOut vert(vsIn v){
				vsOut o;
				o.position = UnityObjectToClipPos(v.position);
				o.normal = v.normal;
				return o;
			}

			float4 frag(vsOut psIn) : SV_TARGET {
				float4 diffuse = saturate(dot(_DiffuseDirection, psIn.normal));
				return (_AmbientLightColor * _AmbientLightIntensity) +
					(diffuse * _DiffuseColor * _DiffuseIntensity);
			}
			ENDCG
		}
	}
}
